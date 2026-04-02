#include <iostream>
#include <string>
#include <limits>

#include "absl/flags/flag.h"
#include "absl/flags/parse.h"
#include "absl/log/check.h"

#include <grpcpp/grpcpp.h>

#include "sensor.grpc.pb.h"

ABSL_FLAG(std::string, address, "0.0.0.0:50051", "Server address");

using grpc::Server;
using grpc::ServerBuilder;
using grpc::ServerContext;
using grpc::ServerReader;
using grpc::Status;
using sensor::SensorData;
using sensor::SensorSummary;
using sensor::SensorService;

////////////////////////////////////////////////////////////////////////////////
//
// Client Streaming 예제: 센서 데이터 수집
//
// 클라이언트가 센서 측정값을 여러 번 스트리밍하면,
// 서버가 모두 수신한 뒤 집계 결과를 1회 응답한다.
//
////////////////////////////////////////////////////////////////////////////////

class SensorServiceImpl final : public SensorService::Service
{
public:
  auto CollectData(ServerContext* context,
                   ServerReader<SensorData>* reader,
                   SensorSummary* summary) -> Status override
  {
    SensorData data;
    int count = 0;
    double sum_temp = 0.0;
    double sum_hum  = 0.0;
    double max_temp = std::numeric_limits<double>::lowest();
    double min_temp = std::numeric_limits<double>::max();

    // 클라이언트가 보내는 만큼 계속 수신
    while (reader->Read(&data))
    {
      double temp = data.temperature();
      double hum  = data.humidity();

      std::cout << "  [" << count << "] sensor=" << data.sensor_id()
                << " temp=" << temp
                << " hum=" << hum << std::endl;

      sum_temp += temp;
      sum_hum  += hum;
      if (temp > max_temp) max_temp = temp;
      if (temp < min_temp) min_temp = temp;
      ++count;
    }

    // 스트림 종료 후 집계 결과 1회 응답
    if (count > 0)
    {
      summary->set_count(count);
      summary->set_avg_temperature(sum_temp / count);
      summary->set_max_temperature(max_temp);
      summary->set_min_temperature(min_temp);
      summary->set_avg_humidity(sum_hum / count);
    }

    std::cout << "Collected " << count << " readings. "
              << "avg_temp=" << summary->avg_temperature() << std::endl;

    return Status::OK;
  }
};

auto main(int argc, char** argv) -> int
{
  absl::ParseCommandLine(argc, argv);

  std::string address = absl::GetFlag(FLAGS_address);

  SensorServiceImpl service;
  ServerBuilder builder;
  builder.AddListeningPort(address, grpc::InsecureServerCredentials());
  builder.RegisterService(&service);

  std::unique_ptr<Server> server(builder.BuildAndStart());
  std::cout << "Sensor server listening on " << address << std::endl;

  server->Wait();

  return 0;
}
