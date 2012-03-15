#include <iostream>
#include <map>
#include <boost/proto/proto.hpp>

using namespace boost::proto;

struct map_insert: callable
{
    typedef void result_type;

    template <typename M, class K, class V> 
    void operator()(M& m, K k, V v) const 
    {
        m[k] = v;
    }
};

struct map_list_of_ {};

struct MapListOf
    : or_ <
        when<terminal<map_list_of_>, _void>, 
        when<
            function<MapListOf, terminal<_>, terminal<_>>, 
            and_ <
                MapListOf(_child0), 
                map_insert(_state, _value(_child1), _value(_child2))
            >
        >
      >
{};

template <typename Expr> 
struct map_list_of_expr;

struct map_list_of_domain
    : domain<pod_generator<map_list_of_expr>, MapListOf>
{};

template <typename Expr> 
struct map_list_of_expr
{
    BOOST_PROTO_EXTENDS(Expr, map_list_of_expr<Expr>, 
        map_list_of_domain)
    template <typename K, class V, class C, class A> 
    operator std::map<K, V, C, A>() const
    {
        BOOST_PROTO_ASSERT_MATCHES(*this, MapListOf);
        std::map<K, V, C, A> map;
        MapListOf()(*this, map);
        return map;
    }
};

map_list_of_expr<terminal<map_list_of_>::type> const map_list_of = {{}};

int main(int argc, char const* argv[])
{
    std::map<int, int> next = map_list_of(1, 2)(2, 3)(3, 4);
    for (auto it = next.begin(); it != next.end(); ++it)
        std::cout << it->second << " ";

    return 0;
}
