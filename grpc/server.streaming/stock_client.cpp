#include <iostream>
#include <string>

#include "absl/flags/flag.h"
#include "absl/flags/parse.h"
#include "absl/log/check.h"

#include <grpcpp/grpcpp.h>

#include "stock.grpc.pb.h"

ABSL_FLAG(std::string, target, "localhost:50051", "Server address");
ABSL_FLAG(std::string, symbol, "AAPL", "Stock symbol to subscribe");

using grpc::Channel;
using grpc::ClientContext;
using grpc::ClientReader;
using grpc::Status;
using stock::StockRequest;
using stock::StockReply;
using stock::StockService;

////////////////////////////////////////////////////////////////////////////////
//
// Server Streaming 클라이언트: 주식 시세 구독
//
// 종목 코드를 1회 전송하고, 서버가 보내는 시세 스트림을 수신한다.
//
//   Client                          Server
//     │── StockRequest("AAPL") ────►│
//     │◄── StockReply($150.10) ─────│  t=0s
//     │◄── StockReply($150.25) ─────│  t=1s
//     │◄── StockReply($149.80) ─────│  t=2s
//     │         ...                  │
//     │◄── Stream 종료 ─────────────│
//
////////////////////////////////////////////////////////////////////////////////

auto main(int argc, char** argv) -> int
{
  absl::ParseCommandLine(argc, argv);

  std::string target = absl::GetFlag(FLAGS_target);
  std::string symbol = absl::GetFlag(FLAGS_symbol);

  auto channel = grpc::CreateChannel(target, grpc::InsecureChannelCredentials());
  auto stub = StockService::NewStub(channel);

  StockRequest request;
  request.set_symbol(symbol);

  ClientContext context;

  // Server streaming: Reader를 통해 서버가 보내는 응답을 반복 수신
  std::unique_ptr<ClientReader<StockReply>> reader(
    stub->SubscribePrice(&context, request));

  StockReply reply;
  std::cout << "Subscribed to " << symbol << " prices:" << std::endl;

  while (reader->Read(&reply))
  {
    std::cout << "  " << reply.symbol()
              << "  $" << reply.price()
              << "  (ts=" << reply.timestamp() << ")"
              << std::endl;
  }

  Status status = reader->Finish();
  if (status.ok())
    std::cout << "Stream completed successfully." << std::endl;
  else
    std::cout << "RPC failed: " << status.error_message() << std::endl;

  return 0;
}
