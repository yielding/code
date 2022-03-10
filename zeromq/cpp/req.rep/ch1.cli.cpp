#include <iostream>
#include <thread>
#include <chrono>
#include <random>
#include <zmq_addon.hpp>

using namespace std;
using namespace chrono_literals;

static zmq::context_t ctx;

int main(int argc, char *argv[])
{
  zmq::socket_t sock(ctx, zmq::socket_type::req);
  sock.connect("tcp://127.0.0.1:5555");

  random_device rd;
  mt19937 mt(rd());
  uniform_int_distribution<int> udist(0, 100);

  while (true) 
  {
    string msg_out = to_string(udist(mt));
    zmq::message_t out(msg_out);
    cout << "\nsending" << out.to_string_view();
    sock.send(out, zmq::send_flags::none);
    zmq::message_t in;
    sock.recv(in);

    cout << "\nsending: " << out
         << " received: " << in.to_string_view();

    this_thread::sleep_for(10ms);
  }

  return 0;
}
