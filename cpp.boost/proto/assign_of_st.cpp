#include <iostream>
#include <map>
#include <cassert>
#include <boost/proto/proto.hpp>

using namespace std;
      namespace proto = boost::proto;

struct map_list_of_ {};
proto::terminal<map_list_of_>::type const map_list_of = {{}};

int main(int argc, char const* argv[])
{
  proto::display_expr(
    map_list_of(1, 2)(3, 4)
  );

  return 0;
}
