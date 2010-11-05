#include <iostream>

#include <boost/type_traits/is_same.hpp>

#include <boost/mpl/transform.hpp>
#include <boost/mpl/lower_bound.hpp>
#include <boost/mpl/less.hpp>
#include <boost/mpl/vector.hpp>
#include <boost/mpl/vector_c.hpp>
#include <boost/mpl/sizeof.hpp>
#include <boost/mpl/deref.hpp>
#include <boost/mpl/transform_view.hpp>

#include <boost/mpl/size.hpp>
#include <boost/mpl/equal.hpp>

#include <boost/mpl/assert.hpp>

namespace mpl = boost::mpl;
using namespace mpl::placeholders;
using namespace std;


typedef mpl::vector<int, char, long long, float> Sequence;

template <class Seq, class MinSize>
struct padded_size0 : mpl::sizeof_ <
    typename mpl::deref<
        typename mpl::lower_bound <
            Seq
            , MinSize       
            , mpl::less<mpl::sizeof_<_1>, _2>
        >::type
    >::type
>
{};

template <typename Seq, typename MinSize>
struct padded_size1 
    :  mpl::deref <
        typename mpl::lower_bound<
            typename mpl::transform<
                Seq, mpl::sizeof_<_>
            >::type
            ,  MinSize
            , mpl::less<_, _>
        >::type
>
{};


template <typename Seq, typename MinSize>
struct padded_size2 
    :  mpl::deref <
            typename mpl::lower_bound<
                typename mpl::transform<
                    Seq, mpl::sizeof_<_>
                >::type
            ,  MinSize
        >::type
    >
{};


template <typename Seq, typename MinSize>
struct first_size_larger_than : mpl::deref <
    typename mpl::lower_bound<
        mpl::transform_view<Seq, mpl::sizeof_<_> >
        , MinSize
    >::type
>
{};


#include <boost/mpl/unpack_args.hpp>
#include <boost/mpl/zip_view.hpp>

typedef mpl::vector_c<int, 1, 2, 3> V1;
typedef mpl::vector_c<int, 4, 5, 6> V2;
typedef mpl::vector_c<int, 7, 8, 9> V3;

typedef mpl::transform_view <
    mpl::zip_view<mpl::vector<V1, V2, V3> >,
    //mpl::unpack_args<mpl::plus<_1, _2, _3> >
    mpl::plus<
        mpl::at<_, mpl::int_<0> >,
        mpl::at<_, mpl::int_<1> >,
        mpl::at<_, mpl::int_<2> >
    >
> ZipedSeq;



#include <boost/mpl/empty_sequence.hpp>
#include <boost/type_traits.hpp>
#include <boost/mpl/begin.hpp>
#include <boost/mpl/end.hpp>

typedef mpl::begin<mpl::empty_sequence>::type first;
typedef mpl::end  <mpl::empty_sequence>::type last;
typedef mpl:: transform_view <
    mpl::empty_sequence,
    boost::add_pointer<_>
> empty_view;
// BOOST_MPL_ASSERT(( boost::is_same<first, last> ));
// BOOST_MPL_ASSERT_RELATION( mpl::size<mpl::empty_sequence>::value, ==, 0);



#include <boost/mpl/joint_view.hpp>
#include <boost/mpl/range_c.hpp>

typedef mpl::joint_view <
    mpl::range_c<int,  0, 10>,
    mpl::range_c<int, 10, 15>
> numbers;
// BOOST_MPL_ASSERT(( mpl::equal< numbers, mpl::range_c<int, 0, 15> > ));


#include <boost/mpl/filter_view.hpp>
#include <boost/mpl/max_element.hpp>

typedef mpl::vector<int, float, float, char[50], long double, char> types;
typedef mpl::max_element<
    mpl::transform_view<
        mpl::filter_view< types,  boost::is_float<_> >, 
        mpl::sizeof_<_> 
    >
>::type iter;
// BOOST_MPL_ASSERT(( boost::is_same< mpl::deref<iter::base>::type, long double > )); 

/*
#include <boost/mpl/sort.hpp>
#include <boost/mpl/back_inserter.hpp>
#include <boost/mpl/copy.hpp>

typedef mpl::vector<float , char[10], char[20]> X1;
typedef mpl::vector<double, char[30], char[40]> X2;
typedef mpl::sort<
    mpl::copy<
        mpl::joint_view<X1, X2>,
        mpl::back_inserter< mpl::vector<_> >
    >::type
>::type result;
*/
#include <boost/mpl/sort.hpp>
#include <boost/mpl/equal.hpp>

typedef mpl::vector_c<int,3,4,0,-5,8,-1,7> numbers_; 
typedef mpl::vector_c<int,-5,-1,0,3,4,7,8> expected; 
typedef mpl::sort<numbers_>::type result_; 
//BOOST_MPL_ASSERT(( equal< result, expected, equal_to<_,_> > )); 


int main (int argc, char * const argv[]) 
{
    // insert code here...	
    /*
    cout << padded_size0<Sequence, mpl::sizeof_<int> >::type::value;
    cout << padded_size1<Sequence, mpl::sizeof_<int> >::type::value;
    cout << padded_size2<Sequence, mpl::sizeof_<int> >::type::value;
    cout << first_size_larger_than<Sequence, mpl::sizeof_<int> >::type::value;
    */
    BOOST_MPL_ASSERT(( mpl::equal< result_, expected, mpl::equal_to<_,_> > ));  
    
    return 0;
}
