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

        cl /nologo /Zm800 /EHsc -I- -I..\..\boost_1_32_0 -c /Fo%TEMP%\metaprogram-appendixA-example21.o example21.cpp

*/
#include <boost/mpl/bool.hpp>

#include <boost/mpl/placeholders.hpp>
#include <boost/mpl/assert.hpp>
#include <boost/static_assert.hpp>
namespace mpl = boost::mpl;
using namespace mpl::placeholders;

struct none {};

#include <boost/preprocessor/seq.hpp>

template <class T>
struct is_integral : mpl::false_ {}; 

// a seq of integral types with unsigned counterparts
#define BOOST_TT_basic_ints            (char)(short)(int)(long)

// generate a seq containing "signed t" and "unsigned t"
#define BOOST_TT_int_pair(r,data,t)      (signed t)(unsigned t)

// a seq of all the integral types
#define BOOST_TT_ints                                           \
    (bool)(char)                                                \
    BOOST_PP_SEQ_FOR_EACH(BOOST_TT_int_pair, ~, BOOST_TT_basic_ints)

// generate an is_integral specialization for type t
#define BOOST_TT_is_integral_spec(r,data,t) \
   template <>                              \
   struct is_integral<t> : mpl::true_ {}; 

BOOST_PP_SEQ_FOR_EACH(BOOST_TT_is_integral_spec, ~, BOOST_TT_ints)

#undef BOOST_TT_is_integral_spec
#undef BOOST_TT_ints
#undef BOOST_TT_int_pair
#undef BOOST_TT_basic_ints
