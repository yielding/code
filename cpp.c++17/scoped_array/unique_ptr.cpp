#include <iostream>
#include <memory>

using namespace std;

struct Test
{
  ~Test()
  {
    cout <<"Test::dtor" <<endl;
  }
};

int main(int argc, char *argv[])
{
  unique_ptr<Test[]> array(new Test[3]);
  
  return 0;
}
