#include <iostream>
#include <vector>
#include <zmq_addon.hpp>

using namespace std;
using namespace zmq;

static context_t ctx;

int main(int argc, char* argv[])
{
  socket_t sock(ctx, socket_type::router);
  sock.bind("tcp://127.0.0.1:5555");

  while (true) 
  {
    vector<message_t> recv_msgs;
    auto res = recv_multipart(sock, 
        back_inserter(recv_msgs));

    if (res.has_value())
      for (auto&& msg: recv_msgs) 
        cout << "\n" << msg;
  }

  return 0;
}
