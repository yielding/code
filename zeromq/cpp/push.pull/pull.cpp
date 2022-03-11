#include <zmq_addon.hpp>
#include <thread>
#include <iostream>

using namespace std;
using namespace zmq;

static context_t ctx;

auto thread_f = [](string id) 
{
  socket_t sock(ctx, socket_type::pull);
  sock.connect("tcp://127.0.0.1:5555");

  while (true) 
  {
    message_t in;
    auto recved = sock.recv(in);

    cout << "\n" 
         << id << " : " 
         << in.to_string_view();
  }
};

int main()
{
  thread th0(thread_f, "th0");
  thread th1(thread_f, "th1");
  thread th2(thread_f, "th2");

  th0.join();
  th1.join();
  th2.join();

  return 0;
}
