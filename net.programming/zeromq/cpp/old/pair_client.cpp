#include <zmq.hpp>
#include <string>
#include <iostream>

// add two integers
int add(int a, int b)
{
  return a + b;
}

int main()
{
  auto c = add(1, 2);

  //  Prepare our context and socket
  zmq::context_t context(1);
  zmq::socket_t  socket(context, ZMQ_PAIR);

  std::cout << "Connecting to hello world server..." << std::endl;
  socket.connect ("tcp://localhost:5555");

  //  Do 10 requests, waiting each time for a response
  for (int request_nbr = 0; request_nbr != 10; request_nbr++) 
  {
    zmq::message_t request (5);
    memcpy (request.data (), "Hello", 5);
    std::cout << "Sending Hello " << request_nbr << "..." << std::endl;
    socket.send (request);

    //  Get the reply.
    zmq::message_t reply;
    socket.recv (&reply);
    std::cout << "Received World " << request_nbr << std::endl;
  }

  return 0;
}
