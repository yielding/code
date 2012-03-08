#include <boost/utility/result_of.hpp>
#include <iostream>

struct _;
 
template<typename Signature>
struct composed_fn;
 
// An improved composed_fn template that uses the TR1 result_of
// protocol to declare and compute function return types.
template<typename Fn1, typename F>
struct composed_fn<Fn1(F)>
{
    // Recursively compose all nested function types to build
    // a composed function object out of them.
    typedef composed_fn<F> Fn2;

    Fn1 fn1;
    Fn2 fn2;
 
    explicit composed_fn(Fn1 f1 = Fn1(), Fn2 f2 = Fn2())
      : fn1(f1), fn2(f2)
    {}
 
    template<typename T>
    auto operator()(T x) const -> decltype(fn1(fn2(x)))
    {
        return fn1(fn2(x));
    }
};
 
// This specialization ends the recursion begun on line 21.
// "composed_fn<Fn(_)> fn; fn(3);" is equivalent to "Fn fn; fn(3);"
template<typename Fn>
struct composed_fn<Fn(_)> : Fn {};
 
// TRICKSY!!! Read below for why this specialization is necessary.
template<typename Fn>
struct composed_fn<Fn *> : composed_fn<Fn> {};

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
struct inc_t
{   
  template <typename T> 
  T operator()(T x) const 
  { 
    return x + 1; 
  }
};

struct square_t
{
  template <typename T> 
  auto operator()(T x) const -> decltype(x*x)
  { 
    return x * x; 
  }
};

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
using namespace std;

int main(int argc, char const* argv[])
{
  composed_fn<inc_t(square_t(inc_t(square_t(_))))> fn;

  cout << fn(3) << endl;
  
  return 0;
}
