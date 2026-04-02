#include <iostream>
#include <string>
#include <chrono>

#include "absl/flags/flag.h"
#include "absl/flags/parse.h"

#include <grpcpp/grpcpp.h>

#include "chat.grpc.pb.h"

using namespace std;

ABSL_FLAG(string, address, "0.0.0.0:50051", "Server address");

using grpc::Server;
using grpc::ServerBuilder;
using grpc::ServerContext;
using grpc::ServerReaderWriter;
using grpc::Status;
using chat::ChatMessage;
using chat::ChatService;

////////////////////////////////////////////////////////////////////////////////
//
// Bidirectional Streaming 예제: 채팅 에코 서버
//
// 클라이언트가 메시지를 보내면 서버가 에코 응답을 보낸다.
// 양방향이 독립적으로 동시에 읽기/쓰기할 수 있다.
//
//   Client                            Server
//     │── "Hello" ──────────────────►│
//     │◄── "[echo] Hello" ───────────│
//     │── "How are you?" ───────────►│
//     │◄── "[echo] How are you?" ────│
//     │── WritesDone() ─────────────►│  Read() returns false
//     │◄── Stream 종료 ──────────────│
//
////////////////////////////////////////////////////////////////////////////////

class ChatServiceImpl final : public ChatService::Service
{
public:
  auto Chat(ServerContext* context,
            ServerReaderWriter<ChatMessage, ChatMessage>* stream) -> Status override
  {
    ChatMessage msg;

    // 클라이언트가 보내는 메시지를 읽으면서 동시에 응답을 쓴다
    while (stream->Read(&msg))
    {
      cout << "[" << msg.user() << "] " << msg.content() << endl;

      auto now = chrono::system_clock::now();
      auto ts  = chrono::duration_cast<chrono::seconds>(
                   now.time_since_epoch()).count();

      ChatMessage reply;
      reply.set_user("server");
      reply.set_content("[echo] " + msg.content());
      reply.set_timestamp(ts);

      stream->Write(reply);
    }

    cout << "Client disconnected." << endl;
    return Status::OK;
  }
};

auto main(int argc, char** argv) -> int
{
  absl::ParseCommandLine(argc, argv);

  string address = absl::GetFlag(FLAGS_address);

  ChatServiceImpl service;
  ServerBuilder builder;
  builder.AddListeningPort(address, grpc::InsecureServerCredentials());
  builder.RegisterService(&service);

  unique_ptr<Server> server(builder.BuildAndStart());
  cout << "Chat server listening on " << address << endl;

  server->Wait();

  return 0;
}
