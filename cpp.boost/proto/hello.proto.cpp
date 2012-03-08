#include <iostream>

#include <boost/proto/proto.hpp>

namespace proto = boost::proto;

using namespace std;

int main(int argc, char const* argv[])
{
  proto::terminal<char const*>::type cout = { "cout" };

  proto::display_expr(cout << "hello" << " " << "Proto!");
  
  return 0;
}
