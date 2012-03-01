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

        cl /nologo /Zm800 /EHsc -I- -I..\..\boost_1_32_0 -c /Fo%TEMP%\metaprogram-chapter7-example4.o example4.cpp

*/
#include <boost/mpl/vector.hpp>
#include <boost/mpl/integral_c.hpp>
#include <boost/mpl/range_c.hpp>
#include <boost/mpl/list_c.hpp>
#include <boost/mpl/contains.hpp>
#include <boost/mpl/transform_view.hpp>
#include <boost/type_traits/remove_cv.hpp>
#include <boost/type_traits/remove_reference.hpp>
#include <boost/mpl/lower_bound.hpp>
#include <boost/mpl/sort.hpp>
#include <boost/mpl/copy.hpp>
#include <boost/mpl/joint_view.hpp>
#include <boost/mpl/back_inserter.hpp>
#include <boost/mpl/deref.hpp>
#include <boost/mpl/equal.hpp>
#include <boost/mpl/vector_c.hpp>
#include <boost/mpl/equal_to.hpp>

#include <boost/mpl/placeholders.hpp>
#include <boost/mpl/assert.hpp>
#include <boost/static_assert.hpp>
namespace mpl = boost::mpl;
using namespace mpl::placeholders;



    typedef mpl::vector<char, long, int const volatile&> seq;

    template <unsigned long N> 
    struct factorial_
    {
        enum { value = N * factorial_<N-1>::value };
        typedef mpl::integral_c<unsigned long, value> type;
    };

    template <>
    struct factorial_<0> : mpl::integral_c<unsigned long, 1>
    {};

    template <class N> 
    struct factorial
      : factorial_<N::value>::type
    {
        BOOST_MPL_AUX_LAMBDA_SUPPORT(1, factorial, (N))
    };
    
    typedef mpl::integral_c<unsigned long,30> n;

    typedef mpl::range_c<int, 15, 18> seq1;
    typedef mpl::list_c<int, 7, 5, 11> seq2;
    

// does seq contain int, int&, int const&, int volatile&, 
// or int const volatile&? 
typedef mpl::contains< 
    mpl::transform_view< 
        seq
      , boost::remove_cv<boost::remove_reference<_> >
    >
  , int
>::type found;

// find the position of the least integer whose factorial is >= n
typedef mpl::lower_bound<
    mpl::transform_view< mpl::range_c<int,0,13>, factorial<_1> >
  , n
>::type::base number_iter;

// return a sorted vector of all the elements from seq1 and seq2
typedef mpl::sort< 
    mpl::copy<
        mpl::joint_view<seq1,seq2>
      , mpl::back_inserter<mpl::vector<> >
    >::type
>::type result;

    BOOST_MPL_ASSERT((found));

    BOOST_STATIC_ASSERT((
      mpl::deref<number_iter>::type::value == 5));

    BOOST_MPL_ASSERT((
      mpl::equal<
          result
        , mpl::vector_c<int,5,7,11,15,16,17>
        , mpl::equal_to<_,_>
      >
    ));

