#include <iostream>
#include <zmq_addon.hpp>

static zmq::context_t ctx;

using namespace std;

int main()
{
  zmq::socket_t sock(ctx, zmq::socket_type::pair);
  sock.bind("tcp://127.0.0.1:5556");

  while (true) 
  {
    zmq::message_t in;
    auto rcved = sock.recv(in);
    cout << in.to_string_view() << endl;

    zmq::message_t out("ok", 2);
    sock.send(out, zmq::send_flags::none);
  }
  

  return 0;
}
