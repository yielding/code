#include <mutex>
#include <thread>
#include <iostream>

using namespace std;

int g_i = 0;
mutex g_mutex;

void safe_increment()
{
  if (lock_guard<mutex> lock(g_mutex); g_i >=0)
  {
    ++g_i;
    cout << this_thread::get_id() << ": " << g_i << '\n';
  }
}

int main(int argc, char *argv[])
{
  cout << __func__ << ": " << g_i << '\n';

  thread t1(safe_increment);
  thread t2(safe_increment);

  t1.join();
  t2.join();

  cout << __func__ << ": " << g_i << '\n';

  return 0;
}
