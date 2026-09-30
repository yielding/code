#include "nats_util.hpp"

#include <chrono>
#include <iostream>
#include <string>
#include <thread>

////////////////////////////////////////////////////////////////////////////////
//
// Core NATS 클라이언트(요청자)
//
//   demo.greet        로 request 를 보내 응답을 기다린다
//   demo.event.tick   으로 fire-and-forget 이벤트를 보낸다
//
////////////////////////////////////////////////////////////////////////////////
namespace
{
  using namespace std;

  constexpr auto request_timeout_ms = int64_t{2000};
  constexpr auto message_count      = 5;
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
    cout << "[core.client] connected to " << url << endl;

    for (auto i = 1; i <= message_count; ++i)
    {
      auto name = "client-" + to_string(i);

      if (auto reply = conn.request("demo.greet", name, request_timeout_ms); reply)
        cout << "[core.client] reply    -> " << data_of(reply.get()) << endl;
      else
        cout << "[core.client] reply    -> (no responder / timeout)" << endl;

      conn.publish("demo.event.tick", "tick " + to_string(i));
      this_thread::sleep_for(chrono::milliseconds(200));
    }

    conn.flush();
    cout << "[core.client] done" << endl;
  }
  catch (const exception& e)
  {
    cerr << "[core.client] error: " << e.what() << endl;
    return 1;
  }

  return 0;
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
