#include <iostream>
#include <future>
#include <thread>

using namespace std;

void worker(promise<void>* p)
{
  this_thread::sleep_for(chrono::seconds(5));
  p->set_value();
}

int main(int argc, char* argv[])
{
  promise<void> p;
  future<void> data = p.get_future();

  thread t(worker, &p);

  while (true)
  {
    auto status = data.wait_for(chrono::seconds(1));
    if (status == future_status::timeout)
    {
      cerr << ">";
    }
    else if (status == future_status::ready)
    {
      cout << " ready" << endl;
      break;
    }
  }

  t.join();

  return 0;
}