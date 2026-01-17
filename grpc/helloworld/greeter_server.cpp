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
#include <absl/strings/str_format.h>
#include <grpcpp/ext/proto_server_reflection_plugin.h>
#include <grpcpp/grpcpp.h>
#include <grpcpp/health_check_service_interface.h>

ABSL_FLAG(uint16_t, port, 50051, "Server port for the service");

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
namespace greeter
{
  using namespace std;

  using grpc::Server;
  using grpc::ServerBuilder;
  using grpc::ServerContext;
  using grpc::Status;
  using helloworld::Greeter;
  using helloworld::HelloReply;
  using helloworld::HelloRequest;

  class greeter_service_impl final : public Greeter::Service
  {
  public:
    auto SayHello(ServerContext* context,
        const HelloRequest* request,
        HelloReply* reply) -> Status override
    {
      string prefix("Hello ");
      reply->set_message(prefix + request->name());

      return Status::OK;
    }
  };

  auto run_server(const uint16_t port) -> void
  {
    string server_address = absl::StrFormat("0.0.0.0:%d", port);
    greeter_service_impl service;

    grpc::EnableDefaultHealthCheckService(true);
    grpc::reflection::InitProtoReflectionServerBuilderPlugin();

    ServerBuilder builder;
    builder.AddListeningPort(server_address, grpc::InsecureServerCredentials());
    builder.RegisterService(&service);

    unique_ptr<Server> server(builder.BuildAndStart());
    cout << "Server listening on " << server_address << endl;

    server->Wait();
  }
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
int main(const int argc, char** argv)
{
  absl::ParseCommandLine(argc, argv);
  greeter::run_server(absl::GetFlag(FLAGS_port));

  return 0;
}
