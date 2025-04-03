#include <net/send_file.hpp>
#include <filesystem>
#include <print>

using namespace std;
      namespace asio = boost::asio;
      namespace fs   = std::filesystem;

using tcp = boost::asio::ip::tcp;


int main(int argc, char *argv[])
{
  if (argc != 3)
    return 1;

  auto port = stoi(argv[1]);
  auto file_path = string{argv[2]};
  if (!fs::exists(file_path))
  {
    println("path: {} does not exist", file_path);
    return 1;
  }

  println("[server] port: {}, path: {}", port, file_path);

  try
  {
    auto io = asio::io_context{};
    auto acceptor = tcp::acceptor(io, tcp::endpoint(tcp::v4(), port));
    auto socket = tcp::socket(io);
    println("[server] listening on port {} ...", port);

    acceptor.accept(socket);
    println("[server] client connected! sending file: {}", file_path);

    core::net::file_sender sender(socket);
    if (!sender.send(file_path))
      println("[server] failed to send file");
  }
  catch (exception& ex)
  {
    println("[server] exception: {}", ex.what());
    return 1;
  }

  return 0;
}
