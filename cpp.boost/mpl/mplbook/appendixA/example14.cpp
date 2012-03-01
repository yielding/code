/*

    Copyright David Abrahams 2003-2004
    Copyright Aleksey Gurtovoy 2003-2004

    Distributed under the Boost Software License, Version 1.0. 
    (See accompanying file LICENSE_1_0.txt or copy at 
    http://www.boost.org/LICENSE_1_0.txt)
            
    This file was automatically extracted from the source of 
    "C++ Template Metaprogramming", by David Abrahams and 
    Aleksey Gurtovoy.

    It is expected to FAIL to preprocess, but does not, with Microsoft
    Visual C++ 6.0 SP6 using the following command:

        cl /nologo /Zm800 /EHsc -I- -I..\..\boost_1_32_0 -E /Fo%TEMP%\metaprogram-appendixA-example14.o example14.cpp

*/
#include <functional>

#include <boost/mpl/placeholders.hpp>
#include <boost/mpl/assert.hpp>
#include <boost/static_assert.hpp>
namespace mpl = boost::mpl;
using namespace mpl::placeholders;

struct none {};

#  include <boost/preprocessor/control/if.hpp>
#  include <boost/preprocessor/comparison/equal.hpp>

// specialization pattern
template <class R BOOST_PP_ENUM_TRAILING_PARAMS(n, class A)>
struct function<R ( BOOST_PP_ENUM_PARAMS(n,A) )>
  BOOST_PP_IF(
      BOOST_PP_EQUAL(n,2), : std::binary_function<A0, A1, R>
    , BOOST_PP_IF(
          BOOST_PP_EQUAL(n,1), : std::unary_function<A0, R>
        , /* ...empty argument... */
      )
  )
{ /* ...class body omitted... */ };
