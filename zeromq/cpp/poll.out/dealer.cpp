#include <iostream>
#include <random>
#include <vector>
#include <thread>
#include <zmq_addon.hpp>
#include <nlohmann/json.hpp>

using namespace std;
using namespace chrono_literals;
using namespace zmq;

static context_t ctx;

int main(int argc, char* argv[])
{
  socket_t sock(ctx, socket_type::dealer);
  sock.set(sockopt::routing_id, "yielding");
  sock.connect("tcp://127.0.0.1:5555");

  pollitem_t items[] = {
    {sock, 0, ZMQ_POLLOUT, 0}
  };

  random_device rd;
  mt19937 mt(rd());
  normal_distribution<double> normdist(0., 1.);

  size_t iter = 0;
  while (true) 
  {
    // -1: infinitive
    //  0: immediate
    //+ve: block ms
    auto rc = poll(items, 1, 0ms);
    if (iter++ % 1000000)
      cout << rc;

    nlohmann::json msg;
    msg["randvar"] = normdist(mt);
    message_t out (msg.dump());
    sock.send(out, send_flags::none);

    // NOTICE: 
    // no this_thread::sleep_for
  }

  return 0;
}
