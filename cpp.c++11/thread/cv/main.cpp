#include <iostream>
#include <string>
#include <thread>
#include <mutex>
#include <condition_variable>

using namespace std;

mutex m;
condition_variable cv;
string data;

bool ready = false;
bool processed = false;

void worker_thread()
{
  unique_lock<mutex> lk(m);

  cv.wait(lk, []{return ready;});

  cout << "Worker thread is processing data\n";

  data += " after processing";

  processed = true;

  cout << "Worker thread signals data processing completed\n";

  lk.unlock();
  cv.notify_one();
}

int main(int argc, const char *argv[])
{
  thread worker(worker_thread);
  data = "Example data";

  {
    lock_guard<mutex> lk(m);
    ready = true;

    cout << "main() signals data ready for processing\n";
  }
  cv.notify_one();

  {
    unique_lock<mutex> lk(m);
    cv.wait(lk, []{return processed;});
  }

  cout << "Back in main(), data = " << data << endl;
  worker.join();
  
  return 0;
}
