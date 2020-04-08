#include "zmq.hpp"
#include <boost/asio.hpp>
#include <boost/asio/thread_pool.hpp>
#include <boost/format.hpp>

#include <iostream>
#include <fstream>
#include <string>

using namespace std;
using namespace boost;
using namespace zmq;

const long CHUNK_SIZE = 25000;
const int  PIPELINE   = 10;

void client_thread0(context_t& ctx, const char* path)
{
  ofstream out(path, ios_base::binary);

  socket_t dealer(ctx, ZMQ_DEALER);
  cout << "socket created" << endl;

  dealer.connect("tcp://172.30.1.21:6001");
  cout << "connect ok" << endl;

  size_t total = 0;       //  Total bytes received
  size_t chunks = 0;      //  Total chunks received

  while (true) 
  {
    dealer.send(buffer("fetch"s), send_flags::sndmore);
    dealer.send(buffer(to_string(total)), send_flags::sndmore);
    dealer.send(buffer(to_string(CHUNK_SIZE)));

    message_t chunk;
    if (auto size = dealer.recv(chunk))
    {
      chunks++;
      out.write(static_cast<char const *>(chunk.data()), *size);
      total += *size;
      if (*size < CHUNK_SIZE)
      {
        cout << "client: get the final chunk. done.." << endl;
        break;
      }
    }
    else
    {
      cout << "read failed" << endl;
      break;
    }
  }

  cout << str(format("%d chunks received, %zd bytes\n") % chunks % total);
}

void client_thread1(context_t& ctx, const char* path)
{
  socket_t dealer(ctx, ZMQ_DEALER);
  dealer.connect("tcp://172.30.1.21:6001");

  size_t credit = PIPELINE;

  size_t total  = 0; // Total bytes received
  size_t chunks = 0; // Total chunks received
  size_t offset = 0; // Offset of next chunk request

  ofstream out(path, ios_base::binary);
  while (true)
  {
    while (credit)
    {
      dealer.send(buffer("fetch"s), send_flags::sndmore);
      dealer.send(buffer(to_string(offset)), send_flags::sndmore);
      dealer.send(buffer(to_string(CHUNK_SIZE)));
      offset += CHUNK_SIZE;
      credit--;
    }

    message_t chunk;
    if (auto size = dealer.recv(chunk))
    {
      chunks++; credit++;
      out.write(static_cast<char const *>(chunk.data()), *size);
      total += *size;
      if (*size < CHUNK_SIZE)
      {
        cout << "client: done." << endl;
        break;
      }
    }
    else
    {
      cout << "read failed" << endl;
      break;
    }
  }

  cout << str(format("%d chunks received, %zd bytes\n") % chunks % total);
}

void server_thread(context_t& ctx, const char* path)
{
  socket_t router(ctx, ZMQ_ROUTER);
  router.bind("tcp://*:6001");

  ifstream in(path, ios_base::binary);

  char buffer[CHUNK_SIZE] = { 0 };
  while (true)
  {
    message_t identity;
    auto s0 = router.recv(identity);
    if (!s0) break;

    message_t message;
    string command;
    if (auto size = router.recv(message))
      command = string((char *)message.data(), *size);

    size_t offset = 0;
    if (auto size = router.recv(message))
      offset = stoi(string((char *)message.data(), *size));

    size_t chunk_sz = 0;
    if (auto size = router.recv(message))
    {
      chunk_sz = stoi(string((char *)message.data(), *size));
      if (chunk_sz == 0)
        break;
    }

    in.seekg(offset, ios_base::beg);
    in.read(buffer, chunk_sz);
    auto actual_read = in.gcount();

    auto to_send = std::min((int)actual_read, (int)chunk_sz);
    router.send(identity, send_flags::sndmore);
    router.send(const_buffer(buffer, to_send), send_flags::none);

    if (actual_read < CHUNK_SIZE)
      break;
  }

  cout << "server done..." << endl;
}

int main(int argc, char *argv[])
{
  auto src = "/Users/yielding/Desktop/IMG_4164.mov";
  auto dst = "/Users/yielding/Desktop/copy.mov";

  // TODO deadline timer
  context_t context(1);

  // client_thread0(&context);
  /** /
  thread server([&] { server_thread (context, src); });
  thread client([&] { client_thread1(context, dst); });
  cout << "1\n";
  server.join();
  cout << "2\n";
  client.join();
  cout << "3\n";
  exit(0);
  / **/

  /**/
  asio::thread_pool pool(4);
  asio::post(pool, [&] { server_thread (context, src); });
  asio::post(pool, [&] { client_thread0(context, dst); });

  pool.join();

  exit(EXIT_SUCCESS);

  return 0;
}
