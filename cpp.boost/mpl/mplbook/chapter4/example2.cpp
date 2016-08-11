/*

    Copyright David Abrahams 2003-2004
    Copyright Aleksey Gurtovoy 2003-2004

    Distributed under the Boost Software License, Version 1.0. 
    (See accompanying file LICENSE_1_0.txt or copy at 
    http://www.boost.org/LICENSE_1_0.txt)
            
    This file was automatically extracted from the source of 
    "C++ Template Metaprogramming", by David Abrahams and 
    Aleksey Gurtovoy.

    It was built successfully with Microsoft Visual C++ 6.0 SP6
    using the following command: 

        cl /nologo /Zm800 /EHsc -I- -I..\..\boost_1_32_0 -c /Fo%TEMP%\metaprogram-chapter4-example2.o example2.cpp

*/
#include <iterator>
#include <boost/mpl/bool.hpp>

#include <boost/mpl/placeholders.hpp>
#include <boost/mpl/assert.hpp>
#include <boost/static_assert.hpp>
namespace mpl = boost::mpl;
using namespace mpl::placeholders;



#include <iterator>

#include <boost/mpl/if.hpp>

struct fast_swap
{
    template <class ForwardIterator1, class ForwardIterator2>
    static void do_it(ForwardIterator1 i1, ForwardIterator2 i2)
    {
        std::swap(*i1, *i2);
    }
};

struct reliable_swap
{
    template <class ForwardIterator1, class ForwardIterator2>
    static void do_it(ForwardIterator1 i1, ForwardIterator2 i2)
    {
        typename 
          std::iterator_traits<ForwardIterator1>::value_type 
        tmp = *i1; 
        *i1 = *i2;
        *i2 = tmp;
    }
};



#include <boost/type_traits/is_reference.hpp>
#include <boost/type_traits/is_same.hpp>
#include <boost/detail/iterator.hpp> // for iterator_traits
#include <utility>   // for swap

namespace nonstd{
 using namespace std;
 using boost::detail::iterator_traits; // the iterator_traits that ships with vc6 is broken
template <bool use_swap> struct iter_swap_impl; // see text

template <class ForwardIterator1, class ForwardIterator2>
void iter_swap(ForwardIterator1 i1, ForwardIterator2 i2)
{
    typedef iterator_traits<ForwardIterator1> traits1;
    typedef typename traits1::value_type v1;
    typedef typename traits1::reference r1;

    typedef iterator_traits<ForwardIterator2> traits2;
    typedef typename traits2::value_type v2;
    typedef typename traits2::reference r2;

    bool const use_swap = boost::is_same<v1,v2>::value
                          && boost::is_reference<r1>::value
                          && boost::is_reference<r2>::value;

mpl::if_<
    mpl::bool_<use_swap>
  , fast_swap
  , reliable_swap
>::type::do_it(i1,i2);

}
} // namespace nonstd

#include <vector>
int main()
{
    bool a[] = {false, true, false};
    std::vector<bool> funky(10);
    nonstd::iter_swap(&a[0],&a[1]);
    nonstd::iter_swap(&a[0],funky.begin());
    nonstd::iter_swap(funky.end()-1,funky.begin());
    return 0;
}

