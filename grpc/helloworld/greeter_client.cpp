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
using grpc::ClientContext;
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

    Status status = _stub->SayHello(&context, request, &reply);

    if (status.ok())
      return reply.message();

    cout << status.error_code() << ": " << status.error_message() << endl;
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
  using namespace std;
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
