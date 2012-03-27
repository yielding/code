#include <boost/proto/proto.hpp>
#include <algorithm>
#include <iostream>
#include <cassert>

using namespace boost;

struct arg1_tag {};

// Forward-declare our custom expression wrapper
template<typename ProtoExpression> struct lambda_expr;
 
// Define a lambda domain and its generator, which
// simply wraps all new expressions our custom wrapper
struct lambda_domain
  : proto::domain<proto::generator<lambda_expr>>
{};

// A Proto "algorithm": a grammar with embedded transforms for
// evaluating lambda expressions (explained in the previous article):
struct Lambda
  : proto::or_<
        // When evaluating a placeholder terminal, return the state.
        proto::when< proto::terminal<arg1_tag>, proto::_state >
        // Otherwise, do the "default" thing.
      , proto::otherwise< proto::_default< Lambda > >
    >
{};

// A lambda is an expression with an operator() that
// evaluates the lambda.
template<typename ProtoExpression>
struct lambda_expr
  : proto::extends<ProtoExpression, lambda_expr<ProtoExpression>, lambda_domain>
{
    lambda_expr(ProtoExpression const &expr = ProtoExpression())
      : lambda_expr::proto_extends(expr)
    {}
 
    // So that boost::result_of can be used to calculate
    // the return type of this lambda expression.
    template<typename Sig> struct result;
 
    template<typename This, typename Arg>
    struct result<This(Arg)>
      : boost::result_of<Lambda(This const &, Arg const &)>
    {};
 
    // Evaluate the lambda expressions
    template<typename Arg1>
    typename result<lambda_expr(Arg1)>::type
    operator()(Arg1 const & arg1) const
    {
        return Lambda()(*this, arg1);
    }
};

// Define arg1 as before, but wrapped in lambda_expr
typedef lambda_expr<proto::terminal<arg1_tag>::type> arg1_type;
arg1_type const arg1 = arg1_type();

// End lambda library here. Begin test code.
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
int main()
{
    int data[10] = { 0, 1};
    std::transform(data + 2, data + 10, data + 2, (&arg1)[-2] + (&arg1)[-1]);
    
    for (int i=0; i<10; i++)
        std::cout << data[i] << " ";
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
