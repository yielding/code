#include <iostream>
#include <zmq_addon.hpp>
#include <nlohmann/json.hpp>
#include <thread>

using namespace zmq;
using namespace std;
using namespace chrono_literals;

static context_t ctx;

int main()
{
  socket_t sock(ctx, socket_type::push);
  sock.bind("tcp://127.0.0.1:5555");

  cout << "connected\n";

  size_t iter = 0;
  while (true) 
  {
    iter++;
    nlohmann::json msg;
    msg["iter"] = iter;

    message_t out(msg.dump());
    sock.send(out, send_flags::none);

    this_thread::sleep_for(500ms);
  }

  return 0;
}
