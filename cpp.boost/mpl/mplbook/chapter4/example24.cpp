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

        cl /nologo /Zm800 /EHsc -I- -I..\..\boost_1_32_0 -c /Fo%TEMP%\metaprogram-chapter4-example24.o example24.cpp

*/
#include <boost/mpl/if.hpp>
#include <boost/mpl/not_equal_to.hpp>
#include <boost/mpl/greater.hpp>
#include <boost/mpl/minus.hpp>
#include <boost/mpl/plus.hpp>
#include <boost/mpl/multiplies.hpp>
#include <boost/mpl/int.hpp>

#include <boost/mpl/placeholders.hpp>
#include <boost/mpl/assert.hpp>
#include <boost/static_assert.hpp>
namespace mpl = boost::mpl;
using namespace mpl::placeholders;


template< typename N1, typename N2 >
struct formula
  : mpl::if_< 
        mpl::not_equal_to<N1,N2>
      , typename mpl::if_< 
            mpl::greater<N1,N2>
          , typename mpl::minus<N1,N2>::type
          , N1
        >::type
      , typename mpl::plus<
            N1
          , typename mpl::multiplies<N1, mpl::int_<2> >::type
        >::type
    >::type
{};

typedef formula<
    mpl::int_<3>, mpl::int_<7>
>::type x;

