#include <iostream>
#include <thread>
#include <atomic>
#include <random>
#include <array>

void update_cur_best(atomic<int>& best, int a, int b) 
{
  if (a < b) 
    a = b;
  
  auto cur_best = best.load(memory_order_relaxed);
  while (cur_best < a && !best.compare_exchange_weak(cur_best, a))
    ;
}

void run(int max, std::atomic<int>& best) 
{
  mt19937 generator{random_device{}()};
  uniform_int_distribution<int> distribution{0, max};
    
  for (int i = 0; i < 15; ++i) 
    update_cur_best(best, distribution(generator), distribution(generator));
}

// g++-9 -std=c++2a ./find.best.cpp
int main()
{
  atomic<int> best{0};
  const int max = 100;
  array<thread, 3> threads;

  for (auto& t : threads) 
    t = thread(run, max, sref(best));

  for (auto& t : threads)
    t.join();

  cout << "best = " << best << endl;
}
