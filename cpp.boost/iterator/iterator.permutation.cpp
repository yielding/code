#include <boost/iterator/permutation_iterator.hpp>
#include <boost/cstdlib.hpp>
#include <iostream>
#include <vector>
#include <deque>
#include <cassert>
#include <algorithm>

using namespace std;
using namespace boost;

ostream_iterator<int> out(cout, " ");

int main() 
{
  int i = 0;

  typedef vector<int> element_range_type;
  typedef deque <int> index_type;

  static const int element_range_size = 10;
  static const int index_size = 4;

  element_range_type elements(element_range_size);
  auto  el_it = elements.begin();
  auto el_end = elements.end();
  for (; el_it != el_end; ++el_it)
    *el_it = distance(elements.begin(), el_it);

  index_type indices(index_size);
  auto  i_it = indices.begin();
  auto i_end = indices.end();
  for (; i_it != i_end; ++i_it) 
    *i_it = element_range_size - index_size + distance(indices.begin(), i_it);

  reverse(indices.begin(), indices.end());

  typedef permutation_iterator<element_range_type::iterator, index_type::iterator> 
    permutation_type;
  permutation_type begin = make_permutation_iterator(elements.begin(), indices.begin());
  permutation_type it    = begin;
  permutation_type end   = make_permutation_iterator(elements.begin(), indices.end());

  cout << "The original range is : ";
  copy(elements.begin(), elements.end(), out);
  cout << "\n";

  cout << "The reindexing scheme is : ";
  copy(indices.begin(), indices.end(), out);
  cout << "\n";

  cout << "The permutated range is : ";
  copy(begin, end, out);
  cout << "\n";

  cout << "Elements at even indices in the permutation : ";
  it = begin;
  for (i=0; i<index_size/2; ++i, it+=2) cout << *it << " ";
  cout << "\n";

  cout << "Permutation backwards : ";
  it = begin + (index_size);
  assert(it != begin);
  for (; it-- != begin ;) cout << *it << " ";
  cout << "\n";

  cout << "Iterate backward with stride 2 : ";
  it = begin + (index_size - 1);
  for (i=0; i<index_size/2; ++i, it-=2) cout << *it << " ";
  cout << "\n";

  return boost::exit_success;
}
