#include <boost/mpl/transform.hpp>
#include <boost/mpl/placeholders.hpp>
#include <boost/mpl/vector_c.hpp>
#include <iostream>

using namespace std;
using namespace boost;
using namespace mpl::placeholders;

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
typedef mpl::vector_c<int, 1, 0, 0, 0, 0, 0, 0> mass;
typedef mpl::vector_c<int, 0, 1, 0, 0, 0, 0, 0> length;
typedef mpl::vector_c<int, 0, 0, 1, 0, 0, 0, 0> time_;
typedef mpl::vector_c<int, 0, 0, 0, 1, 0, 0, 0> charge;
typedef mpl::vector_c<int, 0, 0, 0, 0, 1, 0, 0> temperature;
typedef mpl::vector_c<int, 0, 0, 0, 0, 0, 1, 0> intensity;
typedef mpl::vector_c<int, 0, 0, 0, 0, 0, 0, 1> amount_of_substance;
typedef mpl::vector_c<int, 0, 1,-1, 0, 0, 0, 0> velocity;
typedef mpl::vector_c<int, 0, 1,-2, 0, 0, 0, 0> acceleration;
typedef mpl::vector_c<int, 1, 1,-1, 0, 0, 0, 0> momentum;
typedef mpl::vector_c<int, 1, 1,-2, 0, 0, 0, 0> force;
typedef mpl::vector_c<int, 0, 0, 0, 0, 0, 0, 0> scalar;

template <typename T, class Dimensions> 
struct quantity
{
  explicit quantity(T x) : m_value(x) {}
  T value() const { return m_value; }

private:
  T m_value;
};

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
template <typename T, class D> 
quantity<T, D>
operator+(quantity<T, D> x, quantity<T, D> y)
{
  return quantity<T, D>(x.value() + y.value());
}

struct plus_f
{
  template <typename T1, typename T2> 
  struct apply {
    typedef typename mpl::plus<T1, T2>::type type;
  };
};

template <typename T, class D1, class D2> 
quantity<
  T
 ,typename mpl::transform<D1, D2, plus_f>::type
>
operator*(quantity<T, D1> x, quantity<T, D2> y)
{
  typedef typename mpl::transform<D1, D2, plus_f>::type dim;
  return quantity<T, dim>(x.value() * y.value());
}

template <typename T, class D1, class D2> 
quantity<
  T
 ,typename mpl::transform<D1, D2, mpl::minus<_1, _2> >::type
>
operator/(quantity<T, D1> x, quantity<T, D2> y)
{
  typedef typename mpl::transform<D1, D2, plus_f>::type dim;
  return quantity<T, dim>(x.value() / y.value());
}
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
int main(int argc, char const* argv[])
{
  quantity<float, mass> m(5.0f);
  quantity<float, acceleration> a(9.8f);
  auto res = (m * a).value();
  cout << res;

  return 0;
}
