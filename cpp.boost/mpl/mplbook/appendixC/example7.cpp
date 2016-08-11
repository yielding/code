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

        cl /nologo /Zm800 /EHsc -I- -I..\..\boost_1_32_0 -c /Fo%TEMP%\metaprogram-appendixC-example7.o example7.cpp

*/

#include <boost/mpl/placeholders.hpp>
#include <boost/mpl/assert.hpp>
#include <boost/static_assert.hpp>
#include <boost/mpl/aux_/nttp_decl.hpp>
namespace mpl = boost::mpl;
using namespace mpl::placeholders;



#define N 3

template< BOOST_MPL_AUX_NTTP_DECL(int, i)>
struct deep
{
    template < BOOST_MPL_AUX_NTTP_DECL(int, test) >
    struct inner : deep<i-1>::inner<test>
    {};
};

template< > struct deep<0>
{
    template < BOOST_MPL_AUX_NTTP_DECL(int, test) >
    struct inner { enum { value = 0 }; };
};


template< int n > struct test
{
    enum { value = deep<N>::inner<n>::value };
};

int main()
{
    return test<0>::value + test<1>::value + test<2>::value
        + test<3>::value + test<4>::value + test<5>::value
        + test<6>::value + test<7>::value + test<8>::value
        + test<9>::value;
}
