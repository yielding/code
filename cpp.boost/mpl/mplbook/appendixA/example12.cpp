/*

    Copyright David Abrahams 2003-2004
    Copyright Aleksey Gurtovoy 2003-2004

    Distributed under the Boost Software License, Version 1.0. 
    (See accompanying file LICENSE_1_0.txt or copy at 
    http://www.boost.org/LICENSE_1_0.txt)
            
    This file was automatically extracted from the source of 
    "C++ Template Metaprogramming", by David Abrahams and 
    Aleksey Gurtovoy.

    It was PREPROCESSED successfully with Microsoft Visual C++ 6.0 SP6
    using the following command: 

        cl /nologo /Zm800 /EHsc -I- -I..\..\boost_1_32_0 -E -DTINY_MAX_SIZE=8 /Fo%TEMP%\metaprogram-appendixA-example12.o example12.cpp

*/
#include <boost/mpl/int.hpp>

#include <boost/mpl/placeholders.hpp>
#include <boost/mpl/assert.hpp>
#include <boost/static_assert.hpp>
namespace mpl = boost::mpl;
using namespace mpl::placeholders;

struct none {};

#include <boost/preprocessor/repetition/enum_params.hpp>

#ifndef TINY_MAX_SIZE
#  define TINY_MAX_SIZE 3  // default maximum size is 3
#endif

template <BOOST_PP_ENUM_PARAMS(TINY_MAX_SIZE, class T)>
struct tiny_size
  : mpl::int_<TINY_MAX_SIZE>
{};


#include <boost/preprocessor/repetition.hpp>
#include <boost/preprocessor/arithmetic/sub.hpp>
#include <boost/preprocessor/punctuation/comma_if.hpp>

#define TINY_print(z, n, data) data

#define TINY_size(z, n, unused)                                 \
  template <BOOST_PP_ENUM_PARAMS(n, class T)>                   \
  struct tiny_size<                                             \
      BOOST_PP_ENUM_PARAMS(n,T)                                 \
      BOOST_PP_COMMA_IF(n)                                      \
      BOOST_PP_ENUM(                                            \
          BOOST_PP_SUB(TINY_MAX_SIZE,n), TINY_print, none)      \
  >                                                             \
    : mpl::int_<n> {};




/* // ... */
template < class T0 , class T1 , class T2 , class T3>
struct tiny_size<
    T0 , T1 , T2 , T3
    ,
    none , none , none , none
>
  : mpl::int_<4> {};

template < class T0 , class T1 , class T2 , class T3 , class T4>
struct tiny_size<
    T0 , T1 , T2 , T3 , T4
    ,
    none , none , none
>
  : mpl::int_<5> {};
/* ...etc. */
