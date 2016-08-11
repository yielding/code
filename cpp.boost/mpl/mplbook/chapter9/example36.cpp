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

        cl /nologo /Zm800 /EHsc -I- -I..\..\boost_1_32_0 -c /Fo%TEMP%\metaprogram-chapter9-example36.o example36.cpp

*/

#include <boost/mpl/placeholders.hpp>
#include <boost/mpl/assert.hpp>
#include <boost/mpl/identity.hpp>
#include <boost/mpl/if.hpp>
#include <boost/static_assert.hpp>
namespace mpl = boost::mpl;
using namespace mpl::placeholders;


struct empty_base {};

// MSVC6 has trouble with default template arguments with type void
template <bool> struct always_void { typedef void type; };

template <bool enabled, class T = typename always_void<enabled>::type >
struct enable_if_c
  : mpl::if_c<enabled, mpl::identity<T>, empty_base>
{};

template <class Cond, class T = typename always_void<enabled>::type >
struct enable_if
  : enable_if_c<Cond::value, T>
{};
