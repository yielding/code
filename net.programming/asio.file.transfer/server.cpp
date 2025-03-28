#include <boost/asio.hpp>
#include <iostream>
#include <fstream>
#include <thread>
#include <format>
#include <map>
#include <filesystem>

using boost::asio::ip::tcp;
using namespace std;

void session(tcp::socket socket)
{
  // map<string, ifstream> files;
  
  ifstream file("/Users/yielding/Desktop/data/IMG_0888.MOV", ios::binary);

  try
  {
    vector<char> buffer(16); // [8 byte offset][4 byte size][4 byte filename len]
    vector<char> chunk(25000);

    while (true)
    {
      boost::asio::read(socket, boost::asio::buffer(buffer));
      uint64_t offset = *reinterpret_cast<uint64_t*>(&buffer[0]);
      uint32_t chunk_size = *reinterpret_cast<uint32_t*>(&buffer[8]);
      uint32_t fname_len = *reinterpret_cast<uint32_t*>(&buffer[12]);

      vector<char> fname_buf(fname_len);
      boost::asio::read(socket, boost::asio::buffer(fname_buf));
      string filename{fname_buf.begin(), fname_buf.end()};

      //auto p = format("/Users/yielding/Desktop/data/{}", filename);
      //if (!files.contains(p))
      //{
      //  files[p] = ifstream(p, ios::binary);
      //  if (!files[p].is_open()) continue;
      //}
      // ifstream file(p, ios::binary);
      // if (!file.is_open()) continue;
      chunk.resize(chunk_size);
      //auto& file = files[p];
      file.seekg(offset, ios::beg);
      file.read(chunk.data(), chunk_size);
      streamsize read_bytes = file.gcount();

      boost::asio::write(socket, boost::asio::buffer(chunk.data(), read_bytes));
    }
  } 
  catch (...)
  {
    cout << "[SERVER] Session closed";
  }
}

int main()
{
  try
  {
    boost::asio::io_context io;
    tcp::acceptor acceptor(io, tcp::endpoint(tcp::v4(), 5555));

    cout << "[SERVER] Listening on port 5555";

    while (true)
    {
      tcp::socket socket(io);
      acceptor.accept(socket);
      thread(session, std::move(socket)).detach();
    }
  } 
  catch (exception& e)
  {
    cerr << "Exception: " << e.what() << "\n";
  }
}