#include <iostream>
#include <vector>
#include <thread>
#include <zmq_addon.hpp>

using namespace std;
using namespace zmq;
using namespace chrono_literals;

static context_t ctx;

int main()
{
  socket_t sock(ctx, socket_type::sub);
  sock.set(sockopt::conflate, true);
  sock.connect("tcp://127.0.0.1:5555");

  sock.set(sockopt::subscribe, "");
  //vector<string> topics{"intc", "amd"};
  //for (auto&& topic: topics)
  //{
  //  sock.set(sockopt::subscribe, topic);
  //  //sock.setsockopt(ZMQ_SUBSCRIBE, topic.data(), topic.size());
  //}

  while (true) 
  {
    this_thread::sleep_for(500ms);
    //message_t topic;
    message_t payload;
    //auto r0 = sock.recv(topic);
    auto r1 = sock.recv(payload);

    cout << '\n' 
         //<< topic.to_string_view() << " : "
         << payload.to_string_view()
         << endl;
  }

  return 0;
}
