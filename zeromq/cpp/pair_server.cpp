#include <iostream>
#include <zmq.hpp>
#include <unistd.h>

using namespace std;

int main(int argc, const char * argv[])
{
  zmq::context_t ctx(1);
  zmq::socket_t socket(ctx, ZMQ_PAIR);
  socket.bind("tcp://*:5555");
  
  while (true)
  {
    zmq::message_t request;
    
    socket.recv(&request);
    
    cout << "Received Hello" << endl;
    
    sleep(1);
    
    zmq::message_t reply(5);
    memcpy(reply.data(), "World", 5);
    socket.send(reply);
  }

  return 0;
}
