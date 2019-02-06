#include <iostream>

struct NoChecking
{
  template <typename T>
  static bool check_pointer(T* ) { return true; }
};

struct NullChecking
{
  template <typename T>
  static bool check_pointer(T* p) { return p != nullptr; }
};

struct BadPointerDoNothing 
{
  template <typename T>
  static T* handle_bad_pointer(T* p)
  {
    std::cout << "pointer is moldy" << std::endl;
    return p;
  }
};

template <typename T,
          typename CheckingPolicy=NoChecking,
          typename BadPointerPolicy=BadPointerDoNothing
        >
class pointer_wrapper
{
public:
  pointer_wrapper(): value_(nullptr) 
  {}

  pointer_wrapper(T* p): value_(p) 
  {}

  operator T*()
  {
    if (!CheckingPolicy::check_pointer(value_))
      return BadPointerPolicy::handle_bad_pointer(value_);

    return value_;
  }

private:
  T* value_;
};

using namespace std;

int main(int argc, char *argv[])
{
  pointer_wrapper<int, NullChecking> ptr(new int);
  *ptr = 42;
  cout << "your_number: " << *ptr << endl;

  pointer_wrapper<int, NullChecking> ptr2;
  *ptr2 = 10;
  cout << "my_number: " << *ptr2 << endl;
  
  return 0;
}
