#include <list>
#include <iostream>
#include <iterator>
#include <algorithm>
#include <string>
#include <boost/iterator/function_input_iterator.hpp>

struct generator
{
  typedef int result_type;
  generator() { srand(time(0)); }

  result_type operator()() const {
    return rand();
  }
};

using namespace std;
using namespace boost;

int main(int, char*[])
{
  generator f;
  copy(make_function_input_iterator(f, 0),
       make_function_input_iterator(f, 10),
       ostream_iterator<int>(cout, "\n"));
  copy(make_function_input_iterator(f, infinite()),
       make_function_input_iterator(f, infinite()0),
       ostream_iterator<int>(cout, "\n"));

  return 0;
}
