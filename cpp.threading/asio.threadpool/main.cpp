#include <iostream>
#include <mutex>

#include "sys/active_threadpool.hpp"

using namespace std;

mutex io_mutex;

void hello(int value)
{
  unique_lock l(io_mutex);
  cout << "thead id: " << this_thread::get_id() << " ";
  cout << "hello value : " << value << "\n";
  cout.flush();
}

int main(int argc, char const* argv[])
{
  sys::active_threadpool pool(5);

  int const data_count = 10;
  for (int i=0; i<data_count; i++) 
    pool.dispatch([i]() { hello(i); });

  cout << "made " << data_count << "\n";
  sleep(3);

  //thread th([&pool]() { sleep(3); pool.stop(); });
  //th.join();

  return 0;
}
