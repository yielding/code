#include <iostream>
#include <memory>
#include <string>

using namespace std;

class str
{
public:
  str(string s="def string")
  {
    cout << "def ctor is called" << endl;
    this->s = s;
  }

  str(str const& rhs) 
  {
    cout << "copy ctor is called" << endl;
    if (this != &rhs)
      s = rhs.s;
  }

  str& operator=(str const& rhs)
  {
    cout << "assign operator is called" << endl;

    if (this != &rhs) s = rhs.s;

    return *this;
  }

  str(str && s)
  {
    cout << "move ctor is called" << endl;
  }

  string s;
};

class value 
{
public:
  value(): s("leech") { }

  str&& get_value() { return move(s); }

private:
  str s;
};

int main(int argc, char *argv[])
{
  value v;

  cout << v.get_value().s;
  
  return 0;
}
