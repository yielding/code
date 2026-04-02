#include <iostream>
#include <random>
#include <string>
#include <thread>
#include <chrono>

#include "absl/flags/flag.h"
#include "absl/flags/parse.h"
#include "absl/log/check.h"

#include <grpcpp/grpcpp.h>

#include "sensor.grpc.pb.h"

ABSL_FLAG(std::string, target, "localhost:50051", "Server address");
ABSL_FLAG(std::string, sensor_id, "sensor-001", "Sensor identifier");
ABSL_FLAG(int32_t, count, 20, "Number of readings to send");

using grpc::Channel;
using grpc::ClientContext;
using grpc::ClientWriter;
using grpc::Status;
using sensor::SensorData;
using sensor::SensorSummary;
using sensor::SensorService;

////////////////////////////////////////////////////////////////////////////////
//
// Client Streaming 클라이언트: 센서 데이터 전송
//
// 센서 측정값을 N회 스트리밍하고, 서버로부터 집계 결과를 1회 수신한다.
//
//   Client                             Server
//     │── SensorData(temp=22.1) ──────►│
//     │── SensorData(temp=22.5) ──────►│  reader->Read()
//     │── SensorData(temp=23.0) ──────►│  reader->Read()
//     │── WritesDone() ──────────────►│  Read() returns false
//     │                                │
//     │◄── SensorSummary ─────────────│  집계 결과 1회 응답
//
////////////////////////////////////////////////////////////////////////////////

auto main(int argc, char** argv) -> int
{
  absl::ParseCommandLine(argc, argv);

  std::string target    = absl::GetFlag(FLAGS_target);
  std::string sensor_id = absl::GetFlag(FLAGS_sensor_id);
  int count             = absl::GetFlag(FLAGS_count);

  auto channel = grpc::CreateChannel(target, grpc::InsecureChannelCredentials());
  auto stub = SensorService::NewStub(channel);

  SensorSummary summary;
  ClientContext context;

  // Client streaming: Writer를 통해 데이터를 반복 전송
  std::unique_ptr<ClientWriter<SensorData>> writer(
    stub->CollectData(&context, &summary));

  std::mt19937 rng(std::random_device{}());
  std::uniform_real_distribution<double> temp_dist(20.0, 30.0);
  std::uniform_real_distribution<double> hum_dist(40.0, 80.0);

  std::cout << "Sending " << count << " readings from " << sensor_id
            << "..." << std::endl;

  for (int i = 0; i < count; ++i)
  {
    auto now = std::chrono::system_clock::now();
    auto ts  = std::chrono::duration_cast<std::chrono::seconds>(
                 now.time_since_epoch()).count();

    SensorData data;
    data.set_sensor_id(sensor_id);
    data.set_temperature(temp_dist(rng));
    data.set_humidity(hum_dist(rng));
    data.set_timestamp(ts);

    writer->Write(data);

    std::cout << "  [" << i << "] temp=" << data.temperature()
              << " hum=" << data.humidity() << std::endl;

    std::this_thread::sleep_for(std::chrono::milliseconds(200));
  }

  // 전송 완료 신호
  writer->WritesDone();
  Status status = writer->Finish();

  if (status.ok())
  {
    std::cout << "\nSummary from server:" << std::endl;
    std::cout << "  count:    " << summary.count() << std::endl;
    std::cout << "  avg_temp: " << summary.avg_temperature() << std::endl;
    std::cout << "  max_temp: " << summary.max_temperature() << std::endl;
    std::cout << "  min_temp: " << summary.min_temperature() << std::endl;
    std::cout << "  avg_hum:  " << summary.avg_humidity() << std::endl;
  }
  else
  {
    std::cout << "RPC failed: " << status.error_message() << std::endl;
  }

  return 0;
}
