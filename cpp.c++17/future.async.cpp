#include <iostream>
#include <future>
#include <thread>

using namespace std; 

int main(int argc, char *argv[])
{
  packaged_task<int()> task([](){ return 8; });

  auto f1 = task.get_future();
  thread(move(task)).detach();

  auto f2 = async(launch::async, []() { return 9; });

  promise<int> p;
  auto f3 = p.get_future();
  thread([&p]{ p.set_value_at_thread_exit(10); }).detach();

  cout << "waiting .." << flush;

  f1.wait();
  f2.wait();
  f3.wait();

  cout << "Done!\nResults are: "
       << f1.get() << ' ' << f2.get() << ' ' << f3.get() << '\n';

  return 0;
}
