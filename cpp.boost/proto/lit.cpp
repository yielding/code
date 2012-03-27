#include <iostream>
#include <boost/proto/proto.hpp>

using namespace std;
      namespace proto = boost::proto;

int main(int argc, char const* argv[])
{
    using namespace proto;

    display_expr(lit(0) + 1);
    literal<int> i = 0;

    display_expr(i * 2);
    
    return 0;
}
