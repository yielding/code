#include <iostream>
#include <random>
#include <string>
#include <thread>
#include <chrono>

#include "absl/flags/flag.h"
#include "absl/flags/parse.h"

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
  using std::cout, std::endl;
  namespace chrono = std::chrono;

  absl::ParseCommandLine(argc, argv);

  auto target    = absl::GetFlag(FLAGS_target);
  auto sensor_id = absl::GetFlag(FLAGS_sensor_id);
  auto count     = absl::GetFlag(FLAGS_count);

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

  cout << "Sending " << count << " readings from " << sensor_id
       << "..." << endl;

  for (int i = 0; i < count; ++i)
  {
    auto now = chrono::system_clock::now();
    auto ts  = chrono::duration_cast<chrono::seconds>(
                 now.time_since_epoch()).count();

    SensorData data;
    data.set_sensor_id(sensor_id);
    data.set_temperature(temp_dist(rng));
    data.set_humidity(hum_dist(rng));
    data.set_timestamp(ts);

    writer->Write(data);

    cout << "  [" << i << "] temp=" << data.temperature()
         << " hum=" << data.humidity() << endl;

    std::this_thread::sleep_for(chrono::milliseconds(200));
  }

  // 전송 완료 신호
  writer->WritesDone();
  Status status = writer->Finish();

  if (status.ok())
  {
    cout << "\nSummary from server:" << endl;
    cout << "  count:    " << summary.count() << endl;
    cout << "  avg_temp: " << summary.avg_temperature() << endl;
    cout << "  max_temp: " << summary.max_temperature() << endl;
    cout << "  min_temp: " << summary.min_temperature() << endl;
    cout << "  avg_hum:  " << summary.avg_humidity() << endl;
  }
  else
  {
    cout << "RPC failed: " << status.error_message() << endl;
  }

  return 0;
}
