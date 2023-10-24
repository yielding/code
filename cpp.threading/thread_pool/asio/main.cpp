#include <iostream>
#include <mutex>
#include <thread>

#include <sys/asio_threadpool.hpp>

using namespace std;
using namespace chrono_literals;

mutex io_mutex;

void hello(int value)
{
  unique_lock<mutex> l(io_mutex);
  cerr << "thead id: " << this_thread::get_id() << " ";
  cerr << "hello value : " << value << "\n";
  this_thread::sleep_for(10ms);
}

int main(int argc, char const* argv[])
{
  sys::threadpool pool(5);
  pool.start();

  int const data_count = 1000;
  for (int i=0; i<data_count; i++) 
    pool.post(bind(hello, i));
  //pool.post([i]{ hello(i); });

  cout << "made " << data_count << "\n";

  this_thread::sleep_for(1000ms);

  return 0;
}
