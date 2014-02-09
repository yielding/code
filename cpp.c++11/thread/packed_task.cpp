#include <iostream>
#include <utility>
#include <cmath>
#include <thread>
#include <future>
#include <functional>

using namespace std;


int f(int x, int y)
{
  return pow(x, y);
}

void task_lambda()
{
  packaged_task<int(int, int)> task([](int a, int b) {
      return pow(a, b);
  });

  auto result = task.get_future();
  task(2, 9);

  cout << "task_lambda:\t" << result.get() << endl;
}

void task_bind()
{
  packaged_task<int()> task(bind(f, 2, 11));
  auto result = task.get_future();

  task();
  cout << "task_bind:\t" << result.get() << endl;
}

void task_thread()
{
  packaged_task<int(int, int)> task(f);
  auto result = task.get_future();

  //thread task_thread(std::move(task), 2, 10);
  //task_thread.join();

  //cout << "task_thread:\t" << result.get() << endl;
}

int main(int argc, const char *argv[])
{
  task_lambda();
  task_bind();

  return 0;
}
