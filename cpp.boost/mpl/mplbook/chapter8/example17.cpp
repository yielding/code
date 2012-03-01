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

        cl /nologo /Zm800 /EHsc -I- -I..\..\boost_1_32_0 -c /Fo%TEMP%\metaprogram-chapter8-example17.o example17.cpp

*/
#include <boost/mpl/int.hpp>
#include <boost/mpl/greater_equal.hpp>

#include <boost/mpl/placeholders.hpp>
#include <boost/mpl/assert.hpp>
#include <boost/static_assert.hpp>
namespace mpl = boost::mpl;
using namespace mpl::placeholders;



typedef mpl::int_<0> N;

template <int n>
struct FACTORIAL_requires_NONNEGATIVE_argument
  : mpl::greater_equal<mpl::int_<n>, mpl::int_<0> >
{};
// ...
    BOOST_MPL_ASSERT((
        FACTORIAL_requires_NONNEGATIVE_argument<N::value>));
