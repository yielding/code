#include <iostream>
#include <boost/proto/core.hpp>
#include <boost/proto/context.hpp>

#include <boost/typeof/std/ostream.hpp>

namespace proto = boost::proto;

proto::terminal<std::ostream&>::type cout_ = { std::cout };

template<typename Expr>
void evaluate(Expr const & expr)
{
    proto::default_context ctx;
    proto::eval(expr, ctx);
}

int main()
{
    evaluate( cout_ << "hello" << ',' << " world" );
    return 0;
}
