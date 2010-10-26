#include <iostream>
#include <iterator>
#include <vector>
#include <boost/lambda/lambda.hpp>

using namespace std;

template <typename IntegerType, typename OutputIterator>
void prime_factor(IntegerType m, OutputIterator result)
{
  while (m % 2 == 0) 
  {
    m /= 2;
    *result++ = 2;
  }
  
  if (m > 1) 
  {
    for (IntegerType d = 3; m >= d * d; d += 2)
    while ((m % d) == 0) {
      m /= d;
      *result++ = d;
    }
    if (m > 1) *result++ = m;
  }
}

int main()
{
  using namespace boost::lambda;
  
  int m;
  while (std::cin >> m) 
  {
    std::vector<int> v;
    prime_factor(m, back_inserter(v));
    for_each(v.begin(), v.end(), cout << _1 << ' ');
    cout.put('\n');
  }
  
  return 0;
}