#include "nats_util.hpp"

#include <atomic>
#include <csignal>
#include <iostream>
#include <string>

////////////////////////////////////////////////////////////////////////////////
//
// JetStream 소비자
//
//   durable pull 컨슈머로 DEMO 스트림에서 배치 단위로 당겨와 명시적으로 ack 한다.
//   durable 이므로 프로세스를 껐다 켜도 마지막 ack 다음부터 이어서 받는다.
//   같은 durable 이름으로 여러 개 띄우면 자연스럽게 작업이 분배된다.
//
////////////////////////////////////////////////////////////////////////////////
namespace
{
  using namespace std;

  const auto stream_name    = string{"DEMO"};
  const auto stream_subject = string{"demo.js.>"};
  const auto durable_name   = string{"demo-worker"};

  constexpr auto fetch_batch      = 10;
  constexpr auto fetch_timeout_ms = int64_t{1000};

  atomic g_shutdown{false};

  auto on_signal(const int) -> void
  {
    g_shutdown = true;
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
    auto js   = conn.jet_stream();

    js.ensure_stream(stream_name, {stream_subject});

    auto sub = js.pull_subscribe(stream_subject, durable_name, stream_name);
    cout << "[js.server] connected to " << url << endl;
    cout << "[js.server] pulling " << stream_subject
         << " as durable '" << durable_name << "' (ctrl-c to stop)" << endl;

    auto total = 0;
    while (!g_shutdown)
    {
      auto msgs = sub.fetch(fetch_batch, fetch_timeout_ms);
      for (auto const& msg: msgs)
      {
        cout << "[js.server] consumed <- " << subject_of(msg.get())
             << " : " << data_of(msg.get()) << endl;

        check(natsMsg_Ack(msg.get(), nullptr), "natsMsg_Ack");
        ++total;
      }
    }

    cout << "\n[js.server] acked " << total << " message(s), bye" << endl;
  }
  catch (const exception& e)
  {
    cerr << "[js.server] error: " << e.what() << endl;
    return 1;
  }

  return 0;
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
