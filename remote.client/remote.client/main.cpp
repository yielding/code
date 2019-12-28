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
using namespace zmq;

// 다음에 적당한 상속 구조를 생각해볼 수 있겠다.
//
class command
{
public:
  command(uint32_t c, uint32_t l): m_cmd{c}, m_length{l}
  {
  }
  
  virtual auto message() -> message_t = 0;
  
  virtual auto header() -> message_t
  {
    message_t header(8);
    auto start = (char *)header.data();
    memcpy(start + 0, &m_cmd, 4);
    memcpy(start + 4, &m_length, 4);
    
    return header;
  }

protected:
  uint32_t m_cmd;
  uint32_t m_length;
};

class list_command: public command
{
public:
  list_command(string const& path): command(0, (uint32_t)path.length())
  {
    m_path = path;
  }
  
  auto message() -> message_t override
  {
    return message_t(m_path.data(), m_length);
  }

private:
  string m_path;
};

void split(const string& in, char delim, vector<string>& elems)
{
  stringstream ss(in);
  string item;
  while (getline(ss, item, delim))
    elems.push_back(item);
}

int main(int argc, const char * argv[])
{
  context_t ctx(1);
  socket_t  socket(ctx, ZMQ_REQ);
  socket.connect("tcp://localhost:5555");
  
  string path_to_read = "/Users/yielding/code/cpp.boost";
  list_command cmd(path_to_read);
  
  auto header = cmd.header();
  socket.send(header, send_flags::sndmore);
  
  auto message = cmd.message();
  socket.send(message, send_flags::none);
  
  message_t result;
  if (auto size = socket.recv(result))
  {
    vector<string> elems;
    split(string((char*)result.data(), *size), ';', elems);
    for (auto& elem: elems)
      cout << elem << endl;
  }

  socket.close();
  
  return 0;
}

/*
 message_t m1(5);
 memcpy(m1.data(), "/Users/yielding/code/cpp.boost", 5);
 socket.send(m1, send_flags::sndmore);
 
 message_t m2(5);
 memcpy(m2.data(), "world", 5);
 socket.send(m2, send_flags::none);
 
 if (auto size = socket.recv(m2))
 */
