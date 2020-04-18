#include "zmq.hpp"
#include "file_util.hpp"

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

struct work_context
{
  zmq::context_t* ctx;
  string path;
  string url;
  int64_t chunk_count;
};

void client_thread0(work_context& wc)
{
  ofstream out(wc.path, ios_base::binary);

  socket_t dealer(*wc.ctx, ZMQ_DEALER);
  dealer.connect(wc.url);

  size_t total  = 0;  // total bytes received
  size_t chunks = 0;  // total chunks received

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
        break;
    }
    else
    {
      cout << "read failed" << endl;
      break;
    }
  }

  cout << str(format("%d chunks received, %zd bytes\n") % chunks % total);
}

void client_thread1(work_context& wc)
{
  socket_t dealer(*wc.ctx, ZMQ_DEALER);
  dealer.connect(wc.url.c_str());

  cout << "c: " << wc.url << " connect ok" << endl;

  size_t credit = PIPELINE;

  size_t total  = 0; // total bytes received
  size_t chunks = 0; // total chunks received
  size_t offset = 0; // offset of next chunk request

  // 1. dealer에게 파일의 정보를 얻어온다.
  message_t info; 
  dealer.send(buffer("info"s), send_flags::none);
  if (auto size = dealer.recv(info))
  {
    cout << string((char *)info.data(), *size) << endl;
  }

  // 2. actual file contents transfer
  ofstream out(wc.path.c_str(), ios_base::binary);
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
        cout << "client: done..." << endl;
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

// REMARK
// ROUTER socket이 proxy가 아닌 통신용으로 사용될 경우
// envelop의 구조를 고려해서 코딩한다. 즉, identity 해석
//
void server_thread(work_context& wc)
{
  try
  {
    socket_t router(*(wc.ctx), ZMQ_ROUTER);
    router.bind(wc.url.c_str());

    cout << "s: " << wc.url << " bind ok" << endl;

    ifstream in(wc.path.c_str(), ios_base::binary);

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

      if (command == "info")
      {
        router.send(identity, send_flags::sndmore);
        router.send(const_buffer("use.default", 11), send_flags::none);
      }
      else if (command == "fetch")
      {
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
        in.read (buffer, chunk_sz);
        auto actual_read = in.gcount();

        auto to_send = std::min((int)actual_read, (int)chunk_sz);
        router.send(identity, send_flags::sndmore);
        router.send(const_buffer(buffer, to_send), send_flags::none);

        if (actual_read < CHUNK_SIZE)
          break;
      }
      else
      {
        break;
      }
    }
  }
  catch(error_t e)
  {
    cout << wc.url << endl;
    cout << e.what() << endl;
  }

  cout << "server: done..." << endl;
}

int main(int argc, char *argv[])
{
  auto base = "/Users/yielding/Desktop/"s;

  context_t context(4);

  asio::thread_pool server_pool(4);
  asio::thread_pool client_pool(4);

  vector<string> files = { "1.mp4", "2.mp4", "3.mp4" };
  vector<work_context> sctx;

  int connection_count = 3;

  for (int i=0; i<connection_count; i++)
  {
    work_context server_ctx = { 
      &context, 
      base + files[i],
      "tcp://*:600"s + to_string(i + 1),
      0
    };

    sctx.push_back(server_ctx);
  }

  for (auto& ctx: sctx) 
    asio::post(server_pool, [&] { server_thread (ctx); });

  vector<string> files2 = { "a.mp4", "b.mp4", "c.mp4" };
  vector<work_context> cctx;
  for (int i=0; i<connection_count; i++)
  {
    work_context client_ctx = { 
      &context, 
      base + files2[i],
      "tcp://127.0.0.1:600" + to_string(i + 1),
      0
    };

    cctx.push_back(client_ctx);
  }

  for (auto& ctx: cctx)
    asio::post(client_pool, [&] { client_thread1(ctx); });

  client_pool.join();
  server_pool.join();

  exit(EXIT_FAILURE);

  return 0;
}
