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

        cl /nologo /Zm800 /EHsc -I- -I..\..\boost_1_32_0 -c /Fo%TEMP%\metaprogram-chapter6-example10.o example10.cpp

*/
#include <boost/mpl/vector.hpp>
#include <boost/mpl/vector_c.hpp>
#include <boost/mpl/transform.hpp>
#include <boost/mpl/front.hpp>
#include <boost/mpl/inserter.hpp>
#include <boost/mpl/int.hpp>
#include <boost/mpl/plus.hpp>

#include <boost/mpl/placeholders.hpp>
#include <boost/mpl/assert.hpp>
#include <boost/static_assert.hpp>
namespace mpl = boost::mpl;
using namespace mpl::placeholders;


typedef mpl::vector<
    mpl::vector_c<int, 1, 2, 3>
  , mpl::vector_c<int, 4, 5, 6>
  , mpl::vector_c<int, 7, 8, 9>
> S;

typedef mpl::transform<
    S                   // input sequence
  , mpl::front<_>       // transformation selects front element

  , mpl::inserter<
        mpl::int_<0>    // result starts with 0
      , mpl::plus<_,_>  // and adds each output element.
    >
>::type sum; // 0 + 1 + 4 + 7 == 12

BOOST_STATIC_ASSERT(( sum::value == 12 ));

