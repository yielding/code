/*

    Copyright David Abrahams 2003-2004
    Copyright Aleksey Gurtovoy 2003-2004

    Distributed under the Boost Software License, Version 1.0. 
    (See accompanying file LICENSE_1_0.txt or copy at 
    http://www.boost.org/LICENSE_1_0.txt)
            
    This file was automatically extracted from the source of 
    "C++ Template Metaprogramming", by David Abrahams and 
    Aleksey Gurtovoy.

    It was NOT built successfully with Microsoft Visual C++ 6.0 SP6
    using the following command: 

        cl /nologo /Zm800 /EHsc -I- -I..\..\boost_1_32_0  /Fe%TEMP%\metaprogram-chapter9-example35.exe example35.cpp

*/
#include <vector>

#include <boost/mpl/placeholders.hpp>
#include <boost/mpl/assert.hpp>
#include <boost/static_assert.hpp>
namespace mpl = boost::mpl;
using namespace mpl::placeholders;


#include <iterator>
#include <boost/type_traits/is_arithmetic.hpp>
#include <boost/iterator/iterator_traits.hpp>   
#include <boost/mpl/identity.hpp>
#include <boost/mpl/if.hpp>
#include <boost/mpl/eval_if.hpp>
#include <algorithm>

// This demonstrates some SFINAE techniques that sometimes work with
// VC6... but not in this case.
template <class Iterator, class T = int>
struct enable_if_value_type_is_arithmetic
{
    typedef typename boost::mpl::if_<
        typedef typename boost::is_arithmetic<               // enabling condition
            typename boost::iterator_value<Iterator>::type
        >::type
      , T
      , int&
    >::type type;
};


template <class Iterator, class T>
struct lazy_disable_if_value_type_is_arithmetic
{
    typedef typename boost::mpl::eval_if<
        typedef typename boost::is_arithmetic<               // enabling condition
            typename boost::iterator_value<Iterator>::type
        >::type
      , mpl::identity<int[2]>
      , T
    >::type type;
};

template <class Iterator>
typename boost::iterator_value<Iterator>::type    
sum(Iterator start, Iterator end, typename enable_if_value_type_is_arithmetic<Iterator>::type* = 0)
{
    typename boost::iterator_value<Iterator>::type x(0);
    for (;start != end; ++start)
        x += *start;
    return x;
}

// given an Iterator that points to a container, get the
// value_type of that container's iterators.
template <class Iterator>
struct inner_value
{
    typedef typename boost::detail::iterator_traits<Iterator>::type inner_container;
    typedef typename boost::iterator_value<inner_container>::type type;
};

template <class Iterator>
typename lazy_disable_if_value_type_is_arithmetic<
    Iterator
  , inner_value<Iterator>               // result metafunction
>::type
sum(Iterator start, Iterator end)
{
    typename inner_value<Iterator>::type x(0);

    for (;start != end; ++start)
        x += sum(start->begin(), start->end());

    return x;
}

    #include <boost/iterator/counting_iterator.hpp>
    #include <cassert>
    
    std::vector<double> x;

    int main()
    {
        std::copy(
            boost::make_counting_iterator(3)
          , boost::make_counting_iterator(15)
          , std::back_inserter(x)
        );

        std::vector<std::vector<double> > y(3, x);

        // We don't have a workaround for these failures
        assert(sum(x.begin(), x.end()) == 102);
        assert(sum(y.begin(), y.end()) == 102 * 3);
         return 0;
    }
    

