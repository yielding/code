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

        cl /nologo /Zm800 /EHsc -I- -I..\..\boost_1_32_0 -c /Fo%TEMP%\metaprogram-chapter5-example100.o example100.cpp

*/
#include <boost/mpl/equal.hpp>
#include <boost/mpl/range_c.hpp>
#include <boost/mpl/vector.hpp>
#include <boost/mpl/integral_c.hpp>

#include <boost/mpl/placeholders.hpp>
#include <boost/mpl/assert.hpp>
#include <boost/static_assert.hpp>
namespace mpl = boost::mpl;
using namespace mpl::placeholders;



enum { N = 3, M = 9 };
typedef mpl::equal<mpl::range_c<long,N,M>,

mpl::vector<
    mpl::integral_c<long,N>
  , mpl::integral_c<long,N+1>
  , mpl::integral_c<long,N+2>
  // ...
  , mpl::integral_c<long,M-3>
  , mpl::integral_c<long,M-2>
  , mpl::integral_c<long,M-1> // Note: M-1, not M
>

> compared;
BOOST_MPL_ASSERT((compared));

