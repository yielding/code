#include <net/receive_file.hpp>
#include <sys/timer.hpp>
#include <print>
#include <exception>

namespace asio = boost::asio;

using namespace std;
using namespace core::sys;

int main() 
{
  try 
  {
    asio::io_context io;
    asio::ip::tcp::resolver resolver(io);
    auto endpoints = resolver.resolve("127.0.0.1", "8080");

    asio::ip::tcp::socket socket(io);
    asio::connect(socket, endpoints);

    timer t;

    core::net::file_receiver receiver(socket);
    receiver.read("received.bin");

    println("[client] elapsed: {}", t.elapsed());
  } 
  catch (std::exception& ex) 
  {
    println("[client] exception: {}", ex.what());
    return 1;
  }

  return 0;
}