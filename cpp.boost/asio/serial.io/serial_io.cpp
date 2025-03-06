#include <boost/asio.hpp>
#include <exception>
#include <iostream>

using namespace std;

int main(int argc, char const* argv[])
{
  try 
  {
    using namespace boost::asio;
    io_context io;
    serial_port s(io, "COM4");
    serial_port::baud_rate baud_rate(9600);
    s.set_option(baud_rate);
    deadline_timer t(io);
    unsigned char buf[1] = {0};

    int i = 0;
    while(true)
    {
      s.write_some(buffer(buf, sizeof(buf)));
      cout << static_cast<unsigned int>(buf[0]) << endl;
      buf[0]++;

      cout << ++i << endl;
      t.expires_from_now(boost::posix_time::milliseconds(500));
      t.wait();
    }

    io.run();
  }
  catch(exception& e)
  {
    cerr << e.what() << endl;
  }

  return 0;
}
