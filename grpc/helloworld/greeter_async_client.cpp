/*
 *
 * Copyright 2015 gRPC authors.
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

#include "helloworld.grpc.pb.h"

#include <iostream>
#include <memory>
#include <string>

#include <absl/flags/flag.h>
#include <absl/flags/parse.h>
#include <absl/log/check.h>
#include <grpc/support/log.h>
#include <grpcpp/grpcpp.h>

ABSL_FLAG(std::string, target, "localhost:50051", "Server address");

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
namespace greeter
{

using namespace std;

using grpc::Channel;
using grpc::ClientAsyncResponseReader;
using grpc::ClientContext;
using grpc::CompletionQueue;
using grpc::Status;
using helloworld::Greeter;
using helloworld::HelloReply;
using helloworld::HelloRequest;

class greeter_client
{
public:
  explicit greeter_client(shared_ptr<Channel> channel)
    : _stub(Greeter::NewStub(channel))
  {}

public:
  auto say_hello(const string& user) -> string
  {
    HelloRequest request;
    request.set_name(user);

    HelloReply reply;
    ClientContext context;
    CompletionQueue cq;
    Status status;

    unique_ptr<ClientAsyncResponseReader<HelloReply>> rpc(
      _stub->AsyncSayHello(&context, request, &cq));

    rpc->Finish(&reply, &status, (void*)1);

    void* got_tag;
    bool ok = false;

    CHECK(cq.Next(&got_tag, &ok));
    CHECK_EQ(got_tag, (void*)1);
    CHECK(ok);

    if (status.ok())
      return reply.message();

    return "RPC failed";
  }

private:
  unique_ptr<Greeter::Stub> _stub;
};

}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
int main(const int argc, char** argv)
{
  using namespace greeter;

  absl::ParseCommandLine(argc, argv);

  string target_str = absl::GetFlag(FLAGS_target);
  greeter_client client(
    grpc::CreateChannel(target_str, grpc::InsecureChannelCredentials()));

  string user("world");
  string reply = client.say_hello(user);
  cout << "Greeter received: " << reply << endl;

  return 0;
}
