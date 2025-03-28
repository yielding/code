#include <iostream>
#include <random>
#include <vector>
#include <thread>
#include <zmq_addon.hpp>
#include <nlohmann/json.hpp>

using namespace std;
using namespace chrono_literals;
using namespace zmq;

static context_t ctx;

auto f_send = []() {
  socket_t sock(ctx, socket_type::dealer);
  sock.set(sockopt::routing_id, "kamin_send");
  sock.connect("tcp://127.0.0.1:5555");

  random_device rd;
  mt19937 mt(rd());
  normal_distribution<double> normdist(0., 1.);

  while (true) 
  {
    nlohmann::json msg;
    msg["randvar"] = normdist(mt);
    message_t out (msg.dump());
    sock.send(out, send_flags::none);

    this_thread::sleep_for(1000ms);
  }
};

auto f_recv = []() {
  socket_t sock(ctx, socket_type::dealer);
  sock.set(sockopt::routing_id, "kamin_recv");
  sock.connect("tcp://127.0.0.1:5555");

  while (true) 
  {
    vector<message_t> recv_msgs;
    auto res = recv_multipart(sock, back_inserter(recv_msgs));
    for (auto& msg: recv_msgs)
      cout << "\n" << "kamin" << " " << msg.to_string_view();
  }
};

int main(int argc, char* argv[])
{
  thread th0(f_send);
  thread th1(f_recv);

  th0.join();
  th1.join();

  return 0;
}
