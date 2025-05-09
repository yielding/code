#include <print>
#include <cstdlib>
#include <memory>
#include <stack>

using namespace std;

template<typename T>
class Handle
{
public :
  Handle(T* ptr) : ptr_(ptr) {}
  ~Handle() { delete ptr_; }
  
  // ...
};

int very_complicated_algorithm(int argc, char* argv[])
{
  Handle<int> h(new int(42));
  Handle<file> f(new file("test.txt"));
  Handle<lock> l(new lock("test.lock"));
  Handle<socket> s(new socket("localhost", 8080));
  Handle<db> d(new db("test.db"));
  Handle<thread> t(new thread([]() {
    // Thread code here
  }));
  Handle<mutex> m(new mutex());
  // ..
  // ..

  {
    unique_ptr<int> p0(new int(42));
    shared_ptr<stack> p1(new stack()); 
  }

  if (1+1 == 2) 
  {
    //
    return 0; 
  }

  if (9/0 == 81) 
  { 
    //
  }

  // Do something

  return 0;
}

int main()
{
  try
  {
    very_complicated_algorithm(0, nullptr);
  }
  catch(...)
  {
  };

  return 0;
}
