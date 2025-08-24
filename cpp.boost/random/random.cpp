#include <boost/random.hpp>
#include <boost/iterator/iterator_facade.hpp>

#include <algorithm>
#include <iostream>
#include <vector>

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
using namespace std;
using namespace boost;

template <typename Gen>
class random_number_iterator: 
  public iterator_facade<random_number_iterator<Gen>, 
  typename Gen::result_type const, 
  forward_traversal_tag>
{
public:
  using result_type = typename Gen::result_type;

public:
  explicit random_number_iterator(Gen& gen, const int cnt = 0)
    : _gen(gen)
    , _count(cnt)
    , _value{}
  {
    if (cnt != 0)
      _value = _gen();
  }

public:
  auto increment() -> void
  {
    --_count;
    _value = _gen();
  }

  auto equal(const random_number_iterator& other) const -> bool
  { 
    return _count == other._count; 
  }

  auto dereference() const -> const result_type& 
  { 
    return _value; 
  }

private:
  friend class iterator_core_access;

private:
  Gen& _gen;
  int _count;
  result_type _value;
};

template<typename Gen> 
auto make_random_number_iterator(Gen& gen, const int cnt = 0) -> random_number_iterator<Gen> 
{
  return random_number_iterator<Gen>(gen, cnt);
}

auto main() -> int
{
  mt19937 rng;
  uniform_int_distribution<int> distribution(1, 100);
  variate_generator<mt19937&, uniform_int_distribution<int>> die(rng, distribution);

  vector<int> vi;
  copy(make_random_number_iterator(die, 1000),
      make_random_number_iterator(die),
      back_inserter(vi));

  for (const auto& v : vi) 
    cout << v << " ";

  return 0;
}
