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

        cl /nologo /Zm800 /EHsc -I- -I..\..\boost_1_32_0 -c /Fo%TEMP%\metaprogram-appendixC-example8.o example8.cpp

*/

#include <boost/mpl/placeholders.hpp>
#include <boost/mpl/assert.hpp>
#include <boost/mpl/aux_/nttp_decl.hpp>
#include <boost/static_assert.hpp>
namespace mpl = boost::mpl;
using namespace mpl::placeholders;


template< BOOST_MPL_AUX_NTTP_DECL(int, i)>
struct deep
{
    template < BOOST_MPL_AUX_NTTP_DECL(int, test) >
    struct inner
    {
        enum { value = deep<i-1>::inner<test>::value }; // no forwarding
    };
};

template< > struct deep<0>
{
    template < BOOST_MPL_AUX_NTTP_DECL(int, test) >
    
    struct inner { enum { value = 0 }; };
};


deep<3>::inner<0> x;

