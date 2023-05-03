#include <thread>
#include <atomic>
#include <cassert>
#include <string>

using namespace std;

atomic<string*> ptr;

int data;

void producer()
{
  string* p  = new string("Hello");
  data = 42;
  ptr.store(p, std::memory_order_release);
}

void consumer()
{
  std::string* p2;
  while (!(p2 = ptr.load(std::memory_order_acquire)))
    ;

  assert(*p2 == "Hello"); // never fires
  assert(data == 42); // never fires
}

int main()
{
  thread t1(producer);
  thread t2(consumer);
  t1.join(); 
  t2.join();
}
