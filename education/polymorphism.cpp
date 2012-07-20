#include <iostream>

using namespace std;

class abstract
{
public:
  virtual void freeze() = 0;
};

class concrete: public abstract
{
public:
  virtual void freeze() 
  {
    cout << "I'm concrete\n";
  }
};

class sement: public abstract
{
public:
  virtual void freeze() 
  {
    cout << "I'm sement\n";
  }
};

void do_action(abstract& a)
{
  a.freeze();
}

int main(int argc, const char *argv[])
{
  concrete c;
  sement s;

  do_action(c);
  do_action(s);

  return 0;
}
