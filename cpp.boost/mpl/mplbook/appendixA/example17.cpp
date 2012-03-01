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

        cl /nologo /Zm800 /EHsc -I- -I..\..\boost_1_32_0 -c /Fo%TEMP%\metaprogram-appendixA-example17.o example17.cpp

*/
#include <boost/mpl/vector.hpp>

#include <boost/mpl/placeholders.hpp>
#include <boost/mpl/assert.hpp>
#include <boost/static_assert.hpp>
namespace mpl = boost::mpl;
using namespace mpl::placeholders;

struct none {};


template <class T> struct function;

template <class F> struct signature; // primary template

// partial specializations for boost::function
template <class R>
struct signature<function<R()> > 
  : mpl::vector1<R> {};

template <class R, class A0>
struct signature<function<R(A0)> > 
  : mpl::vector2<R,A0> {};

template <class R, class A0, class A1>
struct signature<function<R(A0,A1)> > 
  : mpl::vector3<R,A0,A1> {};

// ...
