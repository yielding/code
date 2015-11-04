#include <atomic>
#include <iostream>

using namespace std;

int main(int argc, char *argv[])
{
  atomic<int> int_atomic;

  int_atomic = 2;

  int_atomic.fetch_add(1);
  cout << int_atomic << endl;

  int_atomic++;
  cout << int_atomic << endl;

  atomic_fetch_add(&int_atomic, 1);
  cout << int_atomic << endl;
  
  int comparand = 5;
  int new_value = 10;

  auto exchanged = int_atomic.compare_exchange_weak(comparand, new_value);
  cout << int_atomic << endl;

  cout << int_atomic.is_lock_free();

  return 0;
}
