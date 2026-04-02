#include <iostream>
#include <random>
#include <string>
#include <thread>
#include <chrono>

#include "absl/flags/flag.h"
#include "absl/flags/parse.h"
#include "absl/log/check.h"

#include <grpcpp/grpcpp.h>

#include "stock.grpc.pb.h"

ABSL_FLAG(std::string, address, "0.0.0.0:50051", "Server address");

using grpc::Server;
using grpc::ServerBuilder;
using grpc::ServerContext;
using grpc::ServerWriter;
using grpc::Status;
using stock::StockRequest;
using stock::StockReply;
using stock::StockService;

////////////////////////////////////////////////////////////////////////////////
//
// Server Streaming 예제: 주식 시세 구독
//
// 클라이언트가 종목 코드를 1회 요청하면,
// 서버가 1초 간격으로 시세를 10회 스트리밍한다.
//
////////////////////////////////////////////////////////////////////////////////

class StockServiceImpl final : public StockService::Service
{
public:
  auto SubscribePrice(ServerContext* context,
                      const StockRequest* request,
                      ServerWriter<StockReply>* writer) -> Status override
  {
    std::mt19937 rng(std::random_device{}());
    std::uniform_real_distribution<double> dist(-2.0, 2.0);

    double base_price = 150.0;
    auto const& symbol = request->symbol();

    std::cout << "Client subscribed to: " << symbol << std::endl;

    for (int i = 0; i < 10 && !context->IsCancelled(); ++i)
    {
      double price = base_price + dist(rng);
      auto now = std::chrono::system_clock::now();
      auto ts  = std::chrono::duration_cast<std::chrono::seconds>(
                   now.time_since_epoch()).count();

      StockReply reply;
      reply.set_symbol(symbol);
      reply.set_price(price);
      reply.set_timestamp(ts);

      writer->Write(reply);

      std::cout << "  [" << i << "] " << symbol
                << " $" << price << std::endl;

      std::this_thread::sleep_for(std::chrono::seconds(1));
    }

    std::cout << "Stream finished for: " << symbol << std::endl;
    return Status::OK;
  }
};

auto main(int argc, char** argv) -> int
{
  absl::ParseCommandLine(argc, argv);

  std::string address = absl::GetFlag(FLAGS_address);

  StockServiceImpl service;
  ServerBuilder builder;
  builder.AddListeningPort(address, grpc::InsecureServerCredentials());
  builder.RegisterService(&service);

  std::unique_ptr<Server> server(builder.BuildAndStart());
  std::cout << "Stock server listening on " << address << std::endl;

  server->Wait();

  return 0;
}
