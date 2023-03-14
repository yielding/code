#include <iostream>
#include <random>
#include <chrono>
#include <thread>
#include <zmq_addon.hpp>
#include <nlohmann/json.hpp>

using namespace zmq;
using namespace std;
using namespace chrono_literals;

static context_t ctx;

// NOTICE
// connecting two sockets without polling
// a 'dirty' way
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
      cout << "\ndirty: " << in0.to_string_view();

    message_t in1;
    auto r1 = sock1.recv(in1);
    if (r1.has_value())
      cout << "\ndirty: " << in1.to_string_view();

    this_thread::sleep_for(1ms);
  }
};

// NOTICE
// using poll in is 'cleaner' way
//
auto clean = []() {
  socket_t sock0(ctx, socket_type::pull);
  socket_t sock1(ctx, socket_type::pull);
  
  sock0.connect("tcp://127.0.0.1:5555");
  sock1.connect("tcp://127.0.0.1:5556");

  pollitem_t items[] = {
    { sock0, 0, ZMQ_POLLIN, 0 },
    { sock1, 0, ZMQ_POLLIN, 0 }
  };

  while (true) 
  {
    int rc = poll(items, 2, -1ms); // -1: infinite
    message_t in;
    
    if (items[0].revents)
    {
      auto r0 = sock0.recv(in);
      cout << "\nclean: " << in.to_string_view();
    }

    if (items[1].revents)
    {
      auto r1 = sock1.recv(in);
      cout << "\nclean: " << in.to_string_view();
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