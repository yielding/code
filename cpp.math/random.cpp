#include <boost/random.hpp>
#include <boost/iterator/iterator_facade.hpp>
#include <boost/iterator/iterator_traits.hpp>
#include <boost/iterator/iterator_categories.hpp>
#include <iostream>
#include <algorithm>
#include <iterator>
#include <vector>

using namespace std;
using namespace boost;

template<typename Gen>
class random_number_iterator: 
  public iterator_facade<random_number_iterator<Gen>, 
         typename Gen::result_type const, 
         forward_traversal_tag>
{
public:
  typedef typename Gen::result_type result_type;

public:
  explicit random_number_iterator(Gen& gen, int cnt=0)
    : m_gen(gen)
    , m_count(cnt)
  {
    if (cnt != 0)
      m_value = m_gen();
  }

private:
  friend class iterator_core_access;

  void increment()
  {
    --m_count;
    m_value = m_gen();
  }

  bool equal(random_number_iterator const& other) const
  {
    return m_count == other.m_count;
  }

  result_type const& dereference() const { return m_value; }

private:
  Gen& m_gen;
  int  m_count;
  result_type m_value;
};

template<class Gen> random_number_iterator<Gen> 
make_random_number_iterator(Gen& gen, int cnt=0)
{
  return random_number_iterator<Gen>(gen, cnt);
}

int main()
{
  mt19937 rng;
  uniform_int<> six(1, 100);
  variate_generator<mt19937&, uniform_int<>> die(rng, six);

  vector<int> vi;
  copy(make_random_number_iterator(die, 1000),
       make_random_number_iterator(die),
       back_inserter(vi));

  copy(vi.begin(), vi.end(), ostream_iterator<int>(cout, " "));
}
