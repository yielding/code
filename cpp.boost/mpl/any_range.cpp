#include <boost/range/any_range.hpp>

#include <boost/fusion/include/mpl.hpp>
#include <boost/fusion/include/vector.hpp>

#include <boost/mpl/transform.hpp>

#include <vector>
#include <list>


template < typename T >
struct EmbedInAnyRange
{
    typedef boost::any_range< // no need for typename here
        T,                    // no need for typename here
        forward_pass_traversal_tag, 
        int,                  // not sure what this parameter is, I leave int...
        std::ptrdiff_t
    > type;
};

int main()
{
    typedef boost::fusion::vector< double, int, char > Tuple;

    typedef boost::mpl::transform<
        Tuple,
        EmbedInAnyRange< boost::mpl::_ >
    >::type AnyRangeTuple;

    AnyRangeTuple myTuple( 
        std::vector< double >(), 
        std::list< int >(), 
        std::vector< char >() );
}
