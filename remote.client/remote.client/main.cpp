//
//  main.cpp
//  remote.client
//
//  Created by Chang Ha Lee on 2019/11/04.
//  Copyright © 2019 Chang Ha Lee. All rights reserved.
//

#include <iostream>
#include <string>
#include <string_view>
#include <unistd.h>
#include <zmq.hpp>

using namespace std;

int main(int argc, const char * argv[])
{
  zmq::context_t ctx(1);
  zmq::socket_t socket(ctx, ZMQ_REQ);
  socket.connect("tcp://localhost:5555");
  
  string_view data = "foo";
  char buf[3] = { 0 };
  
  for (auto i=0; i<10; i++)
  {
    const auto& d = zmq::buffer(data);
    socket.send(d, zmq::send_flags::none);
    sleep(1);
    
    // zmq::mutable_buffer b(buf, 3);
    // socket.recv(b);
  }
  
  socket.close();
  
  return 0;
}
