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

        cl /nologo /Zm800 /EHsc -I- -I..\..\boost_1_32_0 -c /Fo%TEMP%\metaprogram-chapter8-example25.o example25.cpp

*/
#include <boost/mpl/print.hpp>
#include <boost/mpl/plus.hpp>
#include <boost/mpl/fold.hpp>
#include <boost/mpl/range_c.hpp>
#include <boost/mpl/int.hpp>

#include <boost/mpl/placeholders.hpp>
#include <boost/mpl/assert.hpp>
#include <boost/static_assert.hpp>
namespace mpl = boost::mpl;
using namespace mpl::placeholders;


template <class T, class U>
struct plus_dbg
{
    typedef typename
      mpl::print< typename mpl::plus<T,U>::type >::type
    type;

    BOOST_MPL_AUX_LAMBDA_SUPPORT(2, plus_dbg, (T,U))
};

typedef mpl::fold<
    mpl::range_c<int,1,6>
  , mpl::int_<0>
  , plus_dbg<_1,_2>
>::type sum;
