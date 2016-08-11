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

        cl /nologo /Zm800 /EHsc -I- -I..\..\boost_1_32_0 -c /Fo%TEMP%\metaprogram-appendixC-example5.o example5.cpp

*/

#include <boost/mpl/placeholders.hpp>
#include <boost/mpl/assert.hpp>
#include <boost/static_assert.hpp>
#include <boost/mpl/aux_/nttp_decl.hpp>
namespace mpl = boost::mpl;
using namespace mpl::placeholders;


template<BOOST_MPL_AUX_NTTP_DECL(unsigned, n), BOOST_MPL_AUX_NTTP_DECL(bool, done) = (n < 2)> 
struct lookup
{
    static unsigned const v1
      = lookup<n-1>::value;

    static unsigned const value = v1
#ifndef BASELINE // do memoized lookup
      + lookup<((n%2) ? 0 : n-2)>::value
#endif
      ;
};

template<unsigned n>
struct lookup<n,true>
{
    static unsigned const value = n;
};

lookup<5> x;

