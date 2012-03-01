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

        cl /nologo /Zm800 /EHsc -I- -I..\..\boost_1_32_0 -c /Fo%TEMP%\metaprogram-appendixA-example1.o example1.cpp

*/
#include <boost/mpl/int.hpp>

#include <boost/mpl/placeholders.hpp>
#include <boost/mpl/assert.hpp>
#include <boost/static_assert.hpp>
namespace mpl = boost::mpl;
using namespace mpl::placeholders;

#error This example is just testing a code excerpt that happens to be repeated from earlier in the book. MSVC6/7 don't support partial specialization.  See the workaround file for chapter 5, example 109 for fixes.

struct none {};

template <class T0, class T1, class T2>
struct tiny_size
  : mpl::int_<3> {};


template <class T0, class T1>
struct tiny_size<T0,T1,none>
  : mpl::int_<2> {};

template <class T0>
struct tiny_size<T0,none,none>
  : mpl::int_<1> {};

template <>
struct tiny_size<none,none,none>
  : mpl::int_<0> {};
