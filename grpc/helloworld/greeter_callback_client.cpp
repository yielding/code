/*
 *
 * Copyright 2021 gRPC authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 */

#include <condition_variable>
#include <iostream>
#include <memory>
#include <mutex>
#include <string>

#include "absl/flags/flag.h"
#include "absl/flags/parse.h"

#include <grpcpp/grpcpp.h>

#ifdef BAZEL_BUILD
#include "examples/protos/helloworld.grpc.pb.h"
#else
#include "helloworld.grpc.pb.h"
#endif

using namespace std;

ABSL_FLAG(string, target, "localhost:50051", "Server address");

using grpc::Channel;
using grpc::ClientContext;
using grpc::Status;
using helloworld::Greeter;
using helloworld::HelloReply;
using helloworld::HelloRequest;

class GreeterClient
{
public:
  GreeterClient(shared_ptr<Channel> channel)
    : _stub(Greeter::NewStub(channel)) {}

  // Assembles the client's payload, sends it and presents the response back
  // from the server.
  auto say_hello(const string& user) -> string 
  {
    // Data we are sending to the server.
    HelloRequest request;
    request.set_name(user);

    // Container for the data we expect from the server.
    HelloReply reply;

    // Context for the client. It could be used to convey extra information to
    // the server and/or tweak certain RPC behaviors.
    ClientContext context;

    // The actual RPC.
    mutex mu;
    condition_variable cv;
    bool done = false;
    Status status;
    _stub->async()->SayHello(&context, &request, &reply,
      [&mu, &cv, &done, &status](Status s) -> auto {
        status = std::move(s);
        lock_guard<mutex> lock(mu);
        done = true;
        cv.notify_one();
      }
    );

    unique_lock<mutex> lock(mu);
    while (!done)
      cv.wait(lock);

    // Act upon its status.
    if (status.ok())
      return reply.message();

    cout << status.error_code() << ": " << status.error_message()
              << endl;
    return "RPC failed";
  }

private:
  unique_ptr<Greeter::Stub> _stub;
};

auto main(int argc, char** argv) -> int 
{
  absl::ParseCommandLine(argc, argv);
  // Instantiate the client. It requires a channel, out of which the actual RPCs
  // are created. This channel models a connection to an endpoint specified by
  // the argument "--target=" which is the only expected argument.
  auto target_str = absl::GetFlag(FLAGS_target);
  // We indicate that the channel isn't authenticated (use of
  // InsecureChannelCredentials()).
  GreeterClient greeter(grpc::CreateChannel(target_str, grpc::InsecureChannelCredentials()));
  auto user = "world"s;
  auto reply = greeter.say_hello(user);
  cout << "Greeter received: " << reply << endl;

  return 0;
}
