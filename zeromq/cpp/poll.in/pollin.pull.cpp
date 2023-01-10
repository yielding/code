#include <iostream>
#include <zmq_addon.hpp>
#include <random>
#include <chrono>
#include <thread>
#include <nlohmann/json.hpp>

using namespace std;
using namespace zmq;
using namespace std::chrono_literals;

static context_t ctx;

auto dirty = []() {
  socket_t sock0(ctx, socket_type::pull);
  socket_t sock1(ctx, socket_type::pull);
  
  sock0.connect("tcp://127.0.0.1:5555");
  sock1.connect("tcp://127.0.0.1:5556");

  while (true) 
  {
    message_t in0;
    auto r0 = sock0.recv(in0);
    if (r0.has_value())
      cout << "\n" << in0.to_string_view();

    message_t in1;
    auto r1 = sock1.recv(in1);
    if (r1.has_value())
      cout << "\n" << in1.to_string_view();

    this_thread::sleep_for(1ms);
  }
};

auto clean = []() {
  socket_t sock0(ctx, socket_type::pull);
  socket_t sock1(ctx, socket_type::pull);
  
  sock0.connect("tcp://127.0.0.1:5555");
  sock1.connect("tcp://127.0.0.1:5556");

  pollitem_t items[] = {
    {sock0, 0, ZMQ_POLLIN, 0},
    {sock1, 0, ZMQ_POLLIN, 0}
  };

  while (true) 
  {
    int rc = poll(items, 2, -1ms); // -1: infinite
    message_t in;
    
    if (items[0].revents)
    {
      auto r0 = sock0.recv(in);
      cout << "\n" << in.to_string_view();
    }

    if (items[0].revents)
    {
      auto r1 = sock1.recv(in);
      cout << "\n" << in.to_string_view();
    }
  }
};

int main(int argc, char* argv[])
{
  thread th0(dirty);
  thread th1(clean);

  th0.join();
  th1.join();

  return 0;
}