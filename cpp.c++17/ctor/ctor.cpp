#include <iostream>

using namespace std;

class SomeType 
{
public:
  SomeType() : SomeType(42)
  {
  }

  SomeType(int new_no) : number(new_no)
  {
  }

private:
  int number;
  int value = 5;
};

int main(int argc, const char *argv[])
{
  SomeType st;
  
  return 0;
}
