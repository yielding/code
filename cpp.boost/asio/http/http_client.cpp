#include <iostream>
#include <istream>
#include <ostream>
#include <string>
#include <sstream>
#include <fstream>
#include <cassert>
#include <boost/asio/ip/tcp.hpp>

using boost::asio::ip::tcp;
using namespace std;

int main(int argc, char* argv[])
{
  try
  {
    char const* h = "172.30.1.2";
    char const* q = "/personalportal/pictureview?c=5&id=f9f3ca42-0c31-49fc-9d07-8367e133f7f7";
    boost::asio::ip::tcp::iostream s;

    s.expires_from_now(boost::posix_time::seconds(600));

    // Establish a connection to the server.
    s.connect(h, "8080");
    if (!s)
    {
      cout << "Unable to connect: " << s.error().message() << "\n";
      return 1;
    }

    s << "GET " << q << " HTTP/1.1\r\n";
    s << "Host: " << h << "\r\n";
    s << "Accept: */*\r\n";
    s << "Connection: close\r\n\r\n";

    // Check that response is OK.
    string http_version;      s >> http_version;
    unsigned int status_code; s >> status_code;
    string status_message;
    getline(s, status_message);
    if (!s || http_version.substr(0, 5) != "HTTP/")
    {
      cout << "Invalid response\n";
      return 1;
    }

    if (status_code != 200)
    {
      cout << "Response returned with status code " << status_code << "\n";
      return 1;
    }

    // Process the response headers, which are terminated by a blank line.
    bool chunk_based = false;
    string header;
    while (getline(s, header) && header != "\r")
    {
      if (header.find("chunk") != string::npos)
        chunk_based = true;

      cout << header << "\n";
      cout << flush;
    }

    ofstream out; out.open("x.jpg", ios_base::binary);

    while (true)
    {
      string s_; getline(s, s_);
      size_t to_read = strtol(s_.c_str(), 0, 16);
      cout << "to_read: " << to_read << endl;
      if (to_read == 0)
        break;

      char* buf = new char[to_read+2]; // 2 for 0x0d0a
      s.read(buf, to_read+2);
      assert(to_read+2 == s.gcount());

      out.write(buf, to_read);

      delete [] buf;
    }

    out.close();

    // cerr << s.rdbuf();
  }
  catch (exception& e)
  {
    cout << "Exception: " << e.what() << "\n";
  }

  return 0;
}
