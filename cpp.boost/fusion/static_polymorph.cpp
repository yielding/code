#include <iostream>
#include <string>
#include <boost/fusion/container/vector.hpp>
#include <boost/fusion/algorithm/iteration/for_each.hpp>

using namespace std;
      namespace fusion = boost::fusion;

struct T1
{
  string to_s() { return "T1::to_s()"; }
};

struct T2
{
  string to_s() { return "T2::to_s()"; }
};

struct Functor 
{
  template <typename T> 
  void operator()(T& data) const
  {
    cout << data.to_s() << endl;
  }
};

int main(int argc, char const* argv[])
{
  fusion::vector<T1, T2> container;
  fusion::for_each(container, Functor());
  
  return 0;
}
