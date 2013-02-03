#include <iostream>
#include <vector>
#include <algorithm>
#include <numeric>
#include <future>

using namespace std;

template <typename RAIter>
int parallel_sum(RAIter beg, RAIter end)
{
  typename RAIter::difference_type len = end - beg;
  if (len < 1000)
    return accumulate(beg, end, 0);

  RAIter  mid = beg + len / 2;
  auto handle = async(launch::async, parallel_sum<RAIter>, mid, end);
  int     sum = parallel_sum(beg, mid);

  return sum + handle.get();
}

int main()
{
  vector<int> v(10000, 1);

  cout << "The sum is " 
       << parallel_sum(v.begin(), v.end()) << '\n';
}
