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

        cl /nologo /Zm800 /EHsc -I- -I..\..\boost_1_32_0 -c /Fo%TEMP%\metaprogram-chapter4-example19.o example19.cpp

*/
#include <boost/mpl/bool.hpp>
#include <boost/mpl/long.hpp>
#include <boost/mpl/int.hpp>

#include <boost/mpl/placeholders.hpp>
#include <boost/mpl/assert.hpp>
#include <boost/static_assert.hpp>
namespace mpl = boost::mpl;
using namespace mpl::placeholders;


template<class N1, class N2>
struct equal_to_
//  : mpl::bool_<(N1::value == N2::value)>
// Get the condition out of the template argument list; VC6
// doesn't like it there.
//
{
    enum { value = N1::value == N2::value };
};

template<class N1, class N2>
struct equal_to
  : mpl::bool_<equal_to_<N1,N2>::value>
{};

BOOST_MPL_ASSERT((equal_to<mpl::long_<3>, mpl::int_<3> >));

