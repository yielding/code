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

        cl /nologo /Zm800 /EHsc -I- -I..\..\boost_1_32_0 -c /Fo%TEMP%\metaprogram-chapter8-example20.o example20.cpp

*/
#include <boost/mpl/equal_to.hpp>
#include <boost/type_traits/is_class.hpp>
#include <boost/type_traits/is_integral.hpp>

#include <boost/mpl/placeholders.hpp>
#include <boost/mpl/assert.hpp>
#include <boost/static_assert.hpp>
namespace mpl = boost::mpl;
using namespace mpl::placeholders;


#include <boost/mpl/int.hpp>
#include <boost/mpl/multiplies.hpp>
#include <boost/mpl/equal.hpp>
#include <boost/mpl/eval_if.hpp>
#include <boost/mpl/prior.hpp>
#include <boost/static_assert.hpp>

namespace mpl = boost::mpl;

template <class N>
struct factorial
  : mpl::if_<
        mpl::equal_to<N,mpl::int_<0> >     // check N == 0
      , mpl::int_<1>                    // 0! == 1
      , mpl::multiplies<                // N! == N * (N-1)!
            N
          , factorial<typename mpl::prior<N>::type>
        >
    >::type
{
    

    enum { input = N::value };
    
BOOST_MPL_ASSERT_MSG(
    boost::is_class<N>::value
  , NOT_an_INTEGRAL_CONSTANT_WRAPPER
  , (types<N>));
BOOST_MPL_ASSERT_RELATION(N::value, >=, 0);
BOOST_MPL_ASSERT((boost::is_integral<typename N::value_type>));
BOOST_STATIC_ASSERT(input >= 0); // for nonnegative N
};

factorial<mpl::int_<3> > xxx;

