#include <boost/asio.hpp>
#include <iostream>
#include <chrono>
#include <csignal>
#include <print>
#include <cstdlib>

using namespace std;
namespace asio = boost::asio;

class RepeatingTimer 
{
public:
  RepeatingTimer(asio::io_context& io, chrono::seconds interval, string id)
    : _timer(io, interval)
    , _interval(interval)
    , _count(0)
    , _stopped(false)
    , _id(std::move(id)) 
  {}

  void start() 
  {
    schedule_tick();
  }

  void stop() 
  {
    _stopped = true;
    _timer.cancel();
  }

private:
  void schedule_tick() 
  {
    if (_stopped) 
    {
      println("[{}] Stopped at count = {}", _id, _count);
      return;
    }

    _timer.async_wait(
      [this](const boost::system::error_code& ec) 
      {
        if (!ec) 
        {
          println("[{}] Tick: {}", _id, _count++);
          _timer.expires_after(_interval);
          schedule_tick();  // 재귀 호출
        } 
        else if (ec == boost::asio::error::operation_aborted) 
        {
          println("[{}] Timer operation aborted", _id);
        } 
        else 
        {
          println("[{}] Timer Error: {}", _id, ec.message());
        }
      }
    );
  }

  asio::steady_timer _timer;
  chrono::seconds _interval;
  int _count;
  bool _stopped;
  string _id;
};


int main() 
{
  asio::io_context io;
  asio::signal_set signals(io, SIGINT);

  // 여러 타이머 구성
  vector<shared_ptr<RepeatingTimer>> timers;
  timers.emplace_back(make_shared<RepeatingTimer>(io, chrono::seconds(1), "Timer-A"));
  timers.emplace_back(make_shared<RepeatingTimer>(io, chrono::seconds(2), "Timer-B"));
  timers.emplace_back(make_shared<RepeatingTimer>(io, chrono::seconds(3), "Timer-C"));

  // 타이머 시작
  for (auto& t : timers) {
    t->start();
  }

  // Ctrl+C 처리 → 모든 타이머 정지
  signals.async_wait(
    [&](const boost::system::error_code& ec, int signal_number) 
    {
      if (!ec) 
      {
        std::cout << "\n[Received signal " << signal_number << ", stopping all timers...]" << std::endl;
        for (auto& t : timers) t->stop();
      }
    }
  );

  io.run();
  println("[Main exited cleanly]");

  return 0;
}
