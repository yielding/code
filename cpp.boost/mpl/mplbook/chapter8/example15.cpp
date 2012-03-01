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

        cl /nologo /Zm800 /EHsc -I- -I..\..\boost_1_32_0 -c /Fo%TEMP%\metaprogram-chapter8-example15.o example15.cpp

*/
#include <boost/mpl/greater_equal.hpp>
#include <boost/mpl/int.hpp>
#include <boost/mpl/eval_if.hpp>
#include <boost/mpl/equal_to.hpp>
#include <boost/mpl/multiplies.hpp>
#include <boost/mpl/prior.hpp>

#include <boost/mpl/placeholders.hpp>
#include <boost/mpl/assert.hpp>
#include <boost/mpl/aux_/nttp_decl.hpp>
#include <boost/static_assert.hpp>
namespace mpl = boost::mpl;
using namespace mpl::placeholders;


// specializations are nullary metafunctions that compute n>0
template <BOOST_MPL_AUX_NTTP_DECL(int, n)>
struct FACTORIAL_of_NEGATIVE_NUMBER
  : mpl::greater_equal<mpl::int_<n>, mpl::int_<0> >
{};

template <class N>
struct factorial
  : mpl::eval_if<
        mpl::equal_to<N,mpl::int_<0> >
      , mpl::int_<1>
      , mpl::multiplies<
            N
          , factorial<typename mpl::prior<N>::type>
        >
    >
{
    BOOST_MPL_ASSERT((FACTORIAL_of_NEGATIVE_NUMBER<N::value>));
};

factorial<mpl::int_<0> > x1;

