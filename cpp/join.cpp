#include <iostream>
#include <vector>
#include <sstream>
#include <boost/lambda/lambda.hpp>

using namespace std;

template <typename ForwardIterator>
string join(ForwardIterator first, ForwardIterator last, string const& sep=", ")
{
  using namespace boost::lambda;

  stringstream ss;

  ss << *first;

  for_each(++first, last, ss << constant(sep) << _1);

  return ss.str();
}

int main()
{
  int v[] = {1, 2, 3, 4, 5};

  cout << "[" + join(v, v+ 5) + "]";
}
