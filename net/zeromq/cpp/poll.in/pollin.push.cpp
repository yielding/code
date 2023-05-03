#include <iostream>
#include <random>
#include <chrono>
#include <thread>
#include <zmq_addon.hpp>
#include <nlohmann/json.hpp>

using namespace std;
using namespace chrono_literals;
using namespace zmq;

static context_t ctx;

int main(int argc, char* argv[])
{
  socket_t sock0(ctx, socket_type::push);
  socket_t sock1(ctx, socket_type::push);

  sock0.bind("tcp://127.0.0.1:5555");
  sock1.bind("tcp://127.0.0.1:5556");

  random_device rd;
  mt19937 mt(rd());
  normal_distribution<double> normdist(0., 1.);

  while (true) 
  {
    auto randvar = normdist(mt);
    if (randvar > 0.)
    {
      nlohmann::json msg;
      msg["for sock0"] = randvar;
      message_t out(msg.dump());
      sock0.send(out, send_flags::none);
    }
    else
    {
      nlohmann::json msg;
      msg["for sock1"] = randvar;
      message_t out(msg.dump());
      sock1.send(out, send_flags::none);
    }

    this_thread::sleep_for(500ms);
  }
  
  return 0;
}
