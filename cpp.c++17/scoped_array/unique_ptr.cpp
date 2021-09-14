#include <iostream>
#include <memory>

using namespace std;

struct Test
{
  Test()
  { cout << "default created..\n"; }

  Test(Test const& t)
  { cout << "copied..\n"; }

  Test(Test &&t)
  { cout << "moved..\n"; }

  ~Test()
  { cout << "Test::dtor..\n"; }
};

auto return_array_of_uniq_ptr(int count) -> unique_ptr<Test[]>
{
  unique_ptr<Test[]> array(new Test[count]);

  return array;
}

int main(int argc, char *argv[])
{
  auto arr = return_array_of_uniq_ptr(5);
  
  return 0;
}
