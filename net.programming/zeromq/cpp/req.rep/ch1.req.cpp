#include <thread>
#include <chrono>
#include <random>
#include <print>
#include <zmq_addon.hpp>

using namespace std;
using namespace chrono_literals;

static zmq::context_t ctx;

auto thread_func =[](string thread_id)
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
    println("sending: {}", out.to_string_view());
    sock.send(out, zmq::send_flags::none);
    zmq::message_t in;
    auto recved = sock.recv(in);

    print("thread id: {}, sending: {}  received: ", thread_id, msg_out, in.to_string_view());

    this_thread::sleep_for(500ms);
  }
};

int main(int argc, char *argv[])
{
  thread t0(thread_func, "threadd_0");
  thread t1(thread_func, "threadd_1");

  t0.join();
  t1.join();

  return 0;
}
