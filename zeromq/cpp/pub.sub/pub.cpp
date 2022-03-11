#include <iostream>
#include <random>
#include <chrono>
#include <thread>
#include <vector>
#include <nlohmann/json.hpp>
#include <zmq_addon.hpp>

using namespace std;
using namespace std::chrono_literals;
using namespace zmq;

static context_t ctx;

int main()
{
  socket_t sock(ctx, socket_type::pub);
  sock.bind("tcp://127.0.0.1:5555");

  random_device rd;
  mt19937 mt(rd());
  normal_distribution<double> normdist(0. , 0.1);
  uniform_int_distribution<int> udist(0, 2);

  vector<string> company{"amd", "intc", "aapl"};

  while (true) 
  {
    this_thread::sleep_for(200ms);
    auto select = udist(mt);
    auto name = company[select];
    nlohmann::json msg;
    msg[name] = normdist(mt);

    cout << '\n' << msg.dump();
    
    message_t topic(name);
    sock.send(topic, send_flags::sndmore);
    message_t payload(msg.dump());
    sock.send(payload, send_flags::none);
  }

  return 0;
}
