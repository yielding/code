#include <type_traits>
#include <iostream>

using namespace std;

struct Clear 
{
  int operator()(int) const
  {
    //cout << "Clear::operator()(int)" << endl;
    return 1;
  }

  double operator()(double) const
  {
    //cout << "Clear::operator()(double)" << endl;
    return 0.0;
  }
};

struct Confused 
{
  double operator()(int) const
  {
    //cout << "Clear::operator()(int)" << endl;
    return 1.0;
  }

  int operator()(double) const
  {
    //cout << "Clear::operator()(double)" << endl;
    return 2;
  }
};

template <class Obj>
class Calculus 
{
public:
  template<class Arg> 
  typename result_of<Obj(Arg)>::type 
  operator()(Arg& a) const 
  {
    return member(a);
  }

private:
  Obj member;
};

int main(int argc, const char *argv[])
{
  Calculus<Clear> c0;

  int a = 10;
  double b = 10.0;

  cout << c0(a) << endl;
  cout << c0(b) << endl;

  Calculus<Confused> c1;

  cout << c1(a) << endl;
  cout << c1(b) << endl;

  return 0;
}
