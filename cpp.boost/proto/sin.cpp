#include <iostream>
#include <cmath>
#include <boost/proto/proto.hpp>

using namespace std;
      namespace proto = boost::proto;

template<int I> struct placeholder {};

proto::terminal<placeholder<0>>::type _1;
// make sin function lazy 
proto::terminal<double(*)(double)>::type const sin_ = { &std::sin };

int main(int argc, char const* argv[])
{
    double pi = 3.1415926535;
    proto::default_context ctx;

    cout << proto::eval(sin_(pi/2), ctx) << endl;

    proto::display_expr(
       sin_(pi/2)
    );

    return 0;
}
