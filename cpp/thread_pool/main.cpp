#include <iostream>
#include <boost/thread/mutex.hpp>
#include <boost/timer.hpp>

#include "asio_threadpool.h"

using namespace std;
using namespace boost;

mutex io_mutex;

void hello(int value)
{
  { mutex::scoped_lock l(io_mutex);
    cout << "thead id: " << this_thread::get_id() << " ";
    cout << "hello value : " << value << "\n";
    cout.flush();
    // this_thread::sleep(posix_time::milliseconds(10));
  }
}

int main(int argc, char const* argv[])
{
  sys::threadpool pool(5);
  pool.start();

  int const data_count = 1000;
  for (int i=0; i<data_count; i++) 
    pool.post(bind(hello, i));

  cout << "made " << data_count << "\n";

  sleep(5);

  return 0;
}
