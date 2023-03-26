#include <iostream>
#include <string>
#include <thread> 
#include <mutex>
#include <condition_variable>

using namespace std;

mutex m;
condition_variable cv;
string shared_data;

bool ready = false;
bool processed = false;

void worker_thread()
{
  // Wait until main() sends shared_data
  unique_lock<mutex> lk(m);
  cv.wait(lk, []{ return ready; });

  // after the wait, we own the lock.
  cout << "Worker thread is processing shared_data\n";
  shared_data += " after processing";

  // Send shared_data back to main()
  processed = true;
  cout << "Worker thread signals shared_data processing completed\n";

  // Manual unlocking is done before notifying, to avoid waking up
  // the waiting thread only to block again (see notify_one for details)
  lk.unlock();
  cv.notify_one();
}

int main()
{
  thread worker(worker_thread);

  shared_data = "Example data";
  // send shared_data to the worker thread
  {
    lock_guard<mutex> lk(m);
    ready = true;
    cout << "main() signals shared_data ready for processing\n";
  }
  cv.notify_one();

  // wait for the worker
  {
    unique_lock<mutex> lk(m);
    cv.wait(lk, []{ return processed; });
  }
  cout << "Back in main(), shared_data = " << shared_data << '\n';

  worker.join();
}
