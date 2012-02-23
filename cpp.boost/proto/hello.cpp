#include <iostream>
#include <boost/proto/proto.hpp>
#include <boost/typeof/std/ostream.hpp>

using namespace boost;
using namespace std;

proto::terminal<ostream&>::type cout_ = { std::cout };

template <typename Expr> 
void evaluate(Expr const& expr)
{
  proto::default_context ctx;
  proto::eval(expr, ctx);
}

int main(int argc, char const* argv[])
{
  evaluate(cout_ << "Hello" << ", " << " world");
  
  return 0;
}
