#include <mutex>
#include <thread>
#include <chrono>
#include <iostream>

using namespace std;

struct box
{
  explicit box(int num): num_things(num)
  {}

  int num_things;
  std::mutex m;
};

void transfer(box& from, box& to, int no)
{
  unique_lock<mutex> l1(from.m, std::defer_lock);
  unique_lock<mutex> l2(to.m,   std::defer_lock);

  lock(l1, l2);

  from.num_things -= no;
  to.num_things   += no;

}

int main(int argc, char *argv[])
{
  box acc1(100);
  box acc2(50);

  thread t1(transfer, ref(acc1), ref(acc2), 10);
  thread t2(transfer, ref(acc2), ref(acc1), 5);

  t1.join();
  t2.join();

  cout << acc1.num_things;

  
  return 0;
}
