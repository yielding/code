#include <iostream>
#include <functional>

using namespace std;

struct Foo 
{
  Foo(int num): num_(num) {}

  void print_add(int i) const { cout << num_ + i << endl; }

  int num_;
};

void print_num(int i)
{
  cout << i << endl;
}

int main(int argc, const char * argv[])
{
  function<void(int)> f_display = print_num;
  f_display(-9);

  // function<void()> f_display_42 = []() { print_num(42); };
  auto f_display_42 = []() { print_num(42); };
  f_display_42();

  function<void(Foo const&, int)> f_add_display = &Foo::print_add;
  Foo foo(314159);
  f_add_display(foo, 1);

  return 0;
}
