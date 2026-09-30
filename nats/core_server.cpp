#include "nats_util.hpp"

#include <atomic>
#include <chrono>
#include <csignal>
#include <iostream>
#include <string>
#include <thread>

////////////////////////////////////////////////////////////////////////////////
//
// Core NATS 서버(응답자)
//
//   demo.greet     : request-reply 로 인사말을 돌려준다
//   demo.event.>   : fire-and-forget 이벤트를 받아 로그만 남긴다
//
////////////////////////////////////////////////////////////////////////////////
namespace
{
  using namespace std;
  using namespace natsx;

  atomic g_shutdown{false};

  auto on_signal(const int) -> void
  {
    g_shutdown = true;
  }

  ////////////////////////////////////////////////////////////////////////////////
  //
  // 콜백은 C API가 부르므로 시그니처를 그대로 맞춘다.
  // msg 의 소유권은 콜백에 넘어오므로 반드시 파괴해야 한다.
  //
  ////////////////////////////////////////////////////////////////////////////////
  auto on_greet(natsConnection* nc, natsSubscription*, natsMsg* raw, void*) -> void
  {
    auto msg  = msg_ptr{raw};
    auto name = string(data_of(msg.get()));
    auto res  = "hello, " + (name.empty() ? string("stranger") : name) + "!";

    cout << "[core.server] request  <- " << subject_of(msg.get()) << " : " << name << endl;

    if (auto reply = reply_of(msg.get()); !reply.empty())
      natsConnection_Publish(nc, string(reply).c_str(), res.data(), int(res.size()));
  }

  auto on_event(natsConnection*, natsSubscription*, natsMsg* raw, void*) -> void
  {
    auto msg = msg_ptr{raw};

    cout << "[core.server] event    <- " << subject_of(msg.get())
         << " : " << data_of(msg.get()) << endl;
  }
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

  signal(SIGINT,  on_signal);
  signal(SIGTERM, on_signal);

  try
  {
    auto conn = connection{url};

    auto greet  = conn.subscribe("demo.greet",   on_greet, nullptr);
    auto events = conn.subscribe("demo.event.>", on_event, nullptr);
    conn.flush();

    cout << "[core.server] connected to " << url << endl;
    cout << "[core.server] serving demo.greet, demo.event.> (ctrl-c to stop)" << endl;

    while (!g_shutdown)
      this_thread::sleep_for(chrono::milliseconds(200));

    greet.drain();
    events.drain();
    cout << "\n[core.server] bye" << endl;
  }
  catch (const exception& e)
  {
    cerr << "[core.server] error: " << e.what() << endl;
    return 1;
  }

  return 0;
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
