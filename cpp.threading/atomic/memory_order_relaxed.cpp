#include <vector>
#include <iostream>
#include <thread>
#include <atomic>

using namespace std;

atomic<int> count_ = { 0 };

//
// Relaxed operation: there are no synchronization or ordering constraints, 
// only atomicity is required of this operation.
//
void f()
{
  for (int n=0; n<1000; ++n)
    count_.fetch_add(1, memory_order_relaxed);
}

int main(int argc, char *argv[])
{
  vector<thread> v;

  for (int n=0; n<10; ++n) 
    v.emplace_back(f);

  for (auto&t : v) 
    t.join();

  cout << "final counter value: " << count_ << endl;

  return 0;
}
