#include <iostream>
#include <zmq_addon.hpp>

using namespace std;

static zmq::context_t ctx;

int main(int argc, char *argv[])
{
  zmq::socket_t sock(ctx, zmq::socket_type::rep);
  sock.bind("tcp://127.0.0.1:5555");

  while (true) 
  {
    zmq::message_t in;
    sock.recv(in);

    auto x = stoi(in.to_string());
    auto out = to_string(x*x);
    zmq::message_t zout(out);
    sock.send(zout, zmq::send_flags::none);
  }
  
  return 0;
}
