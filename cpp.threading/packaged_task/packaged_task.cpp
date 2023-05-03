#include <iostream>
#include <future>
#include <thread>

using namespace std;

int main(int argc, char* argv[])
{
  packaged_task<int(int)> task([](int x) { return x + 10; });
  auto start = task.get_future();
  thread t(move(task), 5);

  cout << "result: " << start.get() << endl;

  t.join();

  return 0;
}