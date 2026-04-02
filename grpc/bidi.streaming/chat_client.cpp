#include <iostream>
#include <string>
#include <thread>

#include "absl/flags/flag.h"
#include "absl/flags/parse.h"

#include <grpcpp/grpcpp.h>

#include "chat.grpc.pb.h"

using namespace std;

ABSL_FLAG(string, target, "localhost:50051", "Server address");
ABSL_FLAG(string, user, "alice", "User name");

using grpc::Channel;
using grpc::ClientContext;
using grpc::ClientReaderWriter;
using grpc::Status;
using chat::ChatMessage;
using chat::ChatService;

////////////////////////////////////////////////////////////////////////////////
//
// Bidirectional Streaming 클라이언트: 채팅
//
// 별도 스레드에서 서버 응답을 수신하면서,
// 메인 스레드에서 메시지를 전송한다.
// 양방향이 독립적으로 동시에 동작하는 것이 핵심.
//
////////////////////////////////////////////////////////////////////////////////

auto main(int argc, char** argv) -> int
{
  absl::ParseCommandLine(argc, argv);

  string target = absl::GetFlag(FLAGS_target);
  string user   = absl::GetFlag(FLAGS_user);

  auto channel = grpc::CreateChannel(target, grpc::InsecureChannelCredentials());
  auto stub = ChatService::NewStub(channel);

  ClientContext context;

  // Bidi streaming: ReaderWriter로 동시에 읽기/쓰기
  shared_ptr<ClientReaderWriter<ChatMessage, ChatMessage>> stream(
    stub->Chat(&context));

  // 수신 스레드: 서버로부터 오는 응답을 계속 읽는다
  thread reader_thread([&stream]()
  {
    ChatMessage reply;
    while (stream->Read(&reply))
    {
      cout << "  <- [" << reply.user() << "] "
                << reply.content() << endl;
    }
  });

  // 메인 스레드: 메시지를 전송한다
  vector<string> messages = {
    "Hello!",
    "How are you?",
    "gRPC bidi streaming is great.",
    "Goodbye!"
  };

  for (auto const& text : messages)
  {
    auto now = chrono::system_clock::now();
    auto ts  = chrono::duration_cast<chrono::seconds>(
                 now.time_since_epoch()).count();

    ChatMessage msg;
    msg.set_user(user);
    msg.set_content(text);
    msg.set_timestamp(ts);

    stream->Write(msg);
    cout << "  -> [" << user << "] " << text << endl;

    this_thread::sleep_for(chrono::milliseconds(500));
  }

  // 전송 완료 신호
  stream->WritesDone();

  // 수신 스레드 종료 대기
  reader_thread.join();

  Status status = stream->Finish();
  if (status.ok())
    cout << "Chat ended." << endl;
  else
    cout << "RPC failed: " << status.error_message() << endl;

  return 0;
}
