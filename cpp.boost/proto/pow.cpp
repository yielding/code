#include <boost/proto/proto.hpp>
#include <vector>
#include <iostream>
#include <math.h>
#include <cassert>

namespace mpl    = boost::mpl;
namespace fusion = boost::fusion;
namespace proto  = boost::proto;

using namespace std;
using proto::_;

proto::terminal< std::ostream & >::type cout_ = { std::cout };

template<int I>
struct placeholder
{};

// Define the Protofied placeholder terminals
proto::terminal<placeholder<0> >::type const _1 = {{}};
proto::terminal<placeholder<1> >::type const _2 = {{}};

struct calculator_context
  : proto::callable_context< calculator_context const >
{
    // Values to replace the placeholders
    std::vector<double> args;

    // Define the result type of the calculator.
    // (This makes the calculator_context "callable".)
    typedef double result_type;

    // Handle the placeholders:
    template<int I>
    double operator()(proto::tag::terminal, placeholder<I>) const
    {
        return this->args[I];
    }
};

template<int Exp>
struct pow_fun
{
    typedef double result_type;
    double operator()(double d) const
    {
        return pow(d, Exp);
    }
};

//template<int Exp, typename Arg>
//typename proto::function<
//    typename proto::terminal<pow_fun<Exp> >::type
//  , typename proto::result_of::as_child<Arg const>::type
//>::type const
//pow(Arg const &arg)
//{
//    typedef
//        typename proto::function<
//            typename proto::terminal<pow_fun<Exp> >::type
//          , typename proto::result_of::as_child<Arg const>::type
//        >::type
//    result_type;

//    result_type result = {{{}}, proto::as_child(arg)};
//    return result;
//}

template<int Exp, typename Arg>
typename proto::result_of::make_expr<
    proto::tag::function  // Tag type
  , pow_fun<Exp>          // First child (by value)
  , Arg const &           // Second child (by reference)
>::type const
pow(Arg const &arg)
{
    return proto::make_expr<proto::tag::function>(
        pow_fun<Exp>()    // First child (by value)
      , boost::ref(arg)   // Second child (by reference)
    );
}

int main(int argc, char const* argv[])
{
    /*
    calculator_context ctx;
    ctx.args.push_back(3); // let _1 be 3

    // Create a calculator expression that takes one argument,
    // adds one to it, and raises it to the 2nd power; and then
    // immediately evaluate it using the calculator_context.
    assert( 16 == proto::eval( pow<2>( _1 + 1 ), ctx ) );
    // cout << proto::eval( pow<2>( _1 + 1 ), ctx );
    */

    proto::default_context ctx;

    int a = 4;
    proto::display_expr(pow<2>(a));
    cout << proto::eval(pow<2>(4), ctx);

    return 0;
}

