#include "nats_util.hpp"

#include <chrono>
#include <iostream>
#include <string>
#include <thread>

////////////////////////////////////////////////////////////////////////////////
//
// JetStream 발행자
//
//   DEMO 스트림(subject: demo.js.>)을 보장한 뒤 동기 발행한다.
//   js_Publish 는 서버의 저장 ack 를 기다리므로 core publish 와 달리
//   "저장되었다"는 사실과 스트림 시퀀스를 확인할 수 있다.
//
////////////////////////////////////////////////////////////////////////////////
namespace
{
  using namespace std;

  const auto stream_name    = string{"DEMO"};
  const auto stream_subject = string{"demo.js.>"};
  constexpr auto message_count = 10;
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
auto main(const int argc, const char* argv[]) -> int
{
  using namespace std;
  using namespace natsx;

  auto guard = library_guard{};
  auto url   = argc > 1 ? string(argv[1]) : string(NATS_DEFAULT_URL);

  try
  {
    auto conn = connection{url};
    auto js   = conn.jet_stream();

    js.ensure_stream(stream_name, {stream_subject});
    cout << "[js.client] stream " << stream_name << " ready (" << stream_subject << ")" << endl;

    for (auto i = 1; i <= message_count; ++i)
    {
      auto subject = "demo.js.order";
      auto payload = "order #" + to_string(i);
      auto seq     = js.publish(subject, payload);

      cout << "[js.client] published -> " << subject
           << " : " << payload << " (seq " << seq << ")" << endl;

      this_thread::sleep_for(chrono::milliseconds(100));
    }

    cout << "[js.client] done" << endl;
  }
  catch (const exception& e)
  {
    cerr << "[js.client] error: " << e.what() << endl;
    return 1;
  }

  return 0;
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
