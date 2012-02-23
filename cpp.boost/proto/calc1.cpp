#include <iostream>
#include <boost/proto/core.hpp>
#include <boost/proto/context.hpp>

namespace proto = boost::proto;
using     proto::_;

template <int I> struct placeholder {};

proto::terminal<placeholder<1>>::type const _1 = {{}};
proto::terminal<placeholder<2>>::type const _2 = {{}};

struct calculator_context
  : proto::callable_context<calculator_context const>
{
  double d[2];

  typedef double result_type;

  explicit calculator_context(double d1=0., double d2=0.)
  {
    d[0] = d1;
    d[1] = d2;
  }

  template <int I> 
  double operator()(proto::tag::terminal, placeholder<I>) const
  {
    return d[I-1];
  }
};

template <typename Expr> 
double evaluate(Expr const& expr, double d1=0., double d2=0.)
{
  calculator_context const ctx(d1, d2);

  return proto::eval(expr, ctx);
}

using namespace std;

int main(int argc, char const* argv[])
{
  cout << evaluate( _1 + 2.0, 3.0) << endl;
  cout << evaluate( _1 * _2, 3.0, 2.0) << endl;
  cout << evaluate( (_1 - _2) / _2, 3.0, 2.0) << endl;

  return 0;
}
