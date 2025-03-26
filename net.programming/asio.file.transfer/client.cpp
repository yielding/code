#include <boost/asio.hpp>
#include <iostream>
#include <fstream>
#include "utils.hpp"

using boost::asio::ip::tcp;

using namespace std;

int main()
{
  constexpr size_t CHUNK = 25000;
  constexpr size_t PIPELINE = 10;
  string filename = "IMG_0888.MOV";

  try
  {
    boost::asio::io_context io;
    tcp::socket socket(io);
    socket.connect(tcp::endpoint(boost::asio::ip::make_address("127.0.0.1"), 5555));


    size_t filesize = 91171694;

    ofstream out("recv_" + filename, ios::binary);

    size_t offset = 0;
    size_t credit = PIPELINE;
    size_t total = 0;

    Timer timer;
    while (credit-- && offset < filesize)
    {
      uint64_t off = offset;
      uint32_t size = CHUNK;
      uint32_t fname_len = filename.size();

      vector<char> header(16);
      memcpy(&header[0], &off, 8);
      memcpy(&header[8], &size, 4);
      memcpy(&header[12], &fname_len, 4);

      boost::asio::write(socket, boost::asio::buffer(header));
      boost::asio::write(socket, boost::asio::buffer(filename));

      offset += CHUNK;
    }

    while (total < filesize)
    {
      vector<char> buf(CHUNK);
      size_t n = socket.read_some(boost::asio::buffer(buf));
      out.write(buf.data(), n);
      total += n;
      credit++;

      if (offset < filesize)
      {
        uint64_t off = offset;
        uint32_t size = CHUNK;
        uint32_t fname_len = filename.size();

        vector<char> header(16);
        memcpy(&header[0], &off, 8);
        memcpy(&header[8], &size, 4);
        memcpy(&header[12], &fname_len, 4);

        boost::asio::write(socket, boost::asio::buffer(header));
        boost::asio::write(socket, boost::asio::buffer(filename));

        offset += CHUNK;
        credit--;
      }

      float progress = (total / (float)filesize) * 100.0f;
      cout << "\r[PROGRESS] " << int(progress) << "%" << flush;
    }

    cout << "\n[CLIENT] Done. Received " << total << " bytes\n";
    cout << "\n[CLIENT] Time taken: " << timer.elapsed() << " seconds\n";
  } 
  catch (exception& e)
  {
    cerr << "Exception: " << e.what() << "\n";
  }
}