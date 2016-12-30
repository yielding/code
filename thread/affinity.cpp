#include <iostream>
#include <vector>
#include <thread>
#include <chrono>
#include <sched.h>

using namespace std;

int main(int argc, char *argv[])
{
  constexpr unsigned no_thread = 4;
  mutex iomutex;
  vector<thread> threads(no_thread);

  for (int i=0; i<no_thread; i++)
  {
    threads[i] = thread([&iomutex, i] {
      this_thread::sleep_for(chrono::milliseconds(20));
      while(1) {
        {
          lock_guard<mutex> iolock(iomutex);
          cout << "Thread #" << i << ": on CPU " << sched_getcpu() << endl;
        }

      this_thread::sleep_for(chrono::microseconds(900));
      }
    });

    cpu_set_t cpuset;
    CPU_ZERO(&cpuset);
    CPU_SDT(i, &cpuset);

    int rc = pthread_setaffinity_np(threads[i].native_handle(), 
        sizeof(cpu_set_t), &cpu_set);

    if (rc)
    {
      cerr << "Error calling pthread_setaffinity_np: " << rc << endl;
    }
  }

  for (auto& t: threads) t.join();

  return 0;
}
