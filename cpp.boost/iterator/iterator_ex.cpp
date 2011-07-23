#include <boost/lambda/bind.hpp>
#include <boost/lambda/lambda.hpp>
#include <boost/iterator/iterator_facade.hpp>
#include <boost/iterator/iterator_traits.hpp>
#include <boost/function.hpp>
#include <vector>
#include <list>
#include <iostream>
#include <algorithm>

template <class Iter>
class sorted_view
{
public:
  typedef Iter C_iter;
  typedef typename boost::iterator_value<C_iter>::type C_value_type;
  typedef boost::function<bool (C_value_type const&, C_value_type const&)> Compare;
public:
  class sv_iterator
    : public boost::iterator_facade<sv_iterator, C_value_type const, boost::random_access_traversal_tag>
  {
    typedef typename std::vector<C_iter>::const_iterator S_const_iter;
  public:
    explicit sv_iterator(S_const_iter p)
      : curr_(p) {}

  private:
    friend class boost::iterator_core_access;

    void increment() { ++curr_; }
    void decrement() { --curr_; }

    void advance(typename sv_iterator::difference_type n) {
      std::advance(curr_, n);
    }

    typename sv_iterator::difference_type distance_to(sv_iterator const& to) const {
      return std::distance(curr_, to.curr_);
    }

    bool equal(sv_iterator const& other) const
    {
      return curr_ == other.curr_;
    }

    C_value_type const& dereference() const {
      return **curr_;
    }

    S_const_iter curr_;
  };
public:
  typedef sv_iterator const_iterator;

  sorted_view(C_iter first, C_iter last, Compare comp = std::less<C_value_type>())
    : comp_(comp) 
  {
    using namespace boost::lambda;

    for (C_iter i = first; i != last; ++i)
      data_.push_back(i);

    std::sort(data_.begin(), data_.end(), bind(comp_, *_1, *_2));
  }

  const_iterator begin() const {
    return sv_iterator(data_.begin());
  }

  const_iterator end() const {
    return sv_iterator(data_.end());
  }

private:
  std::vector<C_iter> data_;
  Compare comp_;
};

int main()
{
  using namespace std;
  using namespace boost::lambda;

  list<int> li;
  li.push_back(3);
  li.push_back(0);
  li.push_back(1);
  li.push_back(4);
  li.push_back(2);

  for_each(li.begin(), li.end(), cout << _1 << " ");
  cout << endl;

  typedef sorted_view<list<int>::iterator> sv_t;

  sv_t sv1(li.begin(), li.end());
  for_each(sv1.begin(), sv1.end(), cout << _1 << " ");
  cout << endl;

  sv_t sv2(li.begin(), li.end(), greater<int>());
  for_each(sv2.begin(), sv2.end(), cout << _1 << " ");
  cout << endl;

  sv_t::const_iterator i = sv2.begin();
  i += 2;
  cout << *i << endl;
  cout << i - sv2.begin() << endl;

  int data[] = { 3, 0, 1, 4, 2 };
  int const N = sizeof(data) / sizeof(data[0]);

  sorted_view<int*> sv3(data, data + N);
  for_each(sv3.begin(), sv3.end(), cout << _1 << " ");
  cout << endl;
}
