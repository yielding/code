#include <iostream>
#include <chrono>
#include <random>
#include <thread>
#include <zmq_addon.hpp>
#include <nlohmann/json.hpp>

using namespace std;
using namespace chrono_literals;
using namespace nlohmann;

static zmq::context_t ctx;

int main()
{
  zmq::socket_t sock(ctx, zmq::socket_type::pair);
  sock.connect("tcp://127.0.0.1:5556");

  random_device rd;
  mt19937 mt(rd());
  uniform_int_distribution<int> udist(0, 100);

  while (true) 
  {
    json jmsg;
    jmsg["randint"] = udist(mt);
    zmq::message_t out(jmsg.dump());
    sock.send(out, zmq::send_flags::none);

    this_thread::sleep_for(500ms);

    // NOTICE
    // even though server sends 'ok' we don't have to recv
    // "flexibility"
    //
    // zmq::message_t in;
    // sock.recv(in);
  }

  return 0;
}
