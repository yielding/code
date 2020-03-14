#include <iostream>
#include <memory>

using namespace std;

struct test
{
  ~test()
  {
    cout <<"test::dtor" <<endl;
  }
};

int main(int argc, char *argv[])
{
  unique_ptr<test[]> array(new test[3]);
  
  return 0;
}
