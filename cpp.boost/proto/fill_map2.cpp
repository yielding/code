#include <iostream>
#include <map>
#include <boost/proto/proto.hpp>

using namespace std;
      namespace proto = boost::proto;

struct map_list_of_ {};

proto::terminal<map_list_of_>::type const map_list_of = {{}};

template <typename Map> 
void fill_map(proto::terminal<map_list_of_>::type, Map&)
{}

template <typename Fun, typename Map> 
void fill_map(Fun const& f, Map& m)
{
    fill_map(proto::child_c<0>(f), m);
    m[proto::value(proto::child_c<1>(f))] = proto::value(proto::child_c<2>(f));
}


int main(int argc, char const* argv[])
{
    std::map<int, int> m;
    fill_map(map_list_of(1, 2)(2, 3)(3, 4)(4, 5)(5, 6), m);

    for (auto i = m.begin(); i != m.end(); ++i)
        cout << i->second << " ";

    return 0;
}
