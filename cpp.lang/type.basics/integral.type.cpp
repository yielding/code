#include <iostream>
#include <string>

using namespace std;

class Test
{
public:
  Test()
  {
  }

  void print(char i)
  {
    cout << "char: " << i << endl;
  }

  void print(int i)
  {
    cout << "integer: " << i << endl;
  }

  void print(double d)
  {
    cout << "double: " << d << endl;
  }

  void print(long l)
  {
    cout << "ll: " << sizeof(l) << endl;
  }

  void print(long long l)
  {
    cout << "ll: " << sizeof(l) << endl;
  }

  virtual ~Test()
  {
  }

private:
};

int main(int argc, char const* argv[])
{
  Test t;
  t.print(10);
  t.print(10.);
  t.print('1');
  t.print(10L);
  t.print(10LL);

  return 0;
}
