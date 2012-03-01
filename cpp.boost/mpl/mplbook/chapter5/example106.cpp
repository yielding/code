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

        cl /nologo /Zm800 /EHsc -I- -I..\..\boost_1_32_0 -c /Fo%TEMP%\metaprogram-chapter5-example106.o example106.cpp

*/
#include <boost/mpl/vector.hpp>
#include <boost/mpl/eval_if.hpp>
#include <boost/mpl/empty.hpp>
#include <boost/mpl/identity.hpp>
#include <boost/mpl/pop_front.hpp>
#include <boost/type_traits/is_same.hpp>

#include <boost/mpl/placeholders.hpp>
#include <boost/mpl/assert.hpp>
#include <boost/static_assert.hpp>
namespace mpl = boost::mpl;
using namespace mpl::placeholders;


struct none {}; // tag type to denote no element

template <class T0 = none, class T1 = none, class T2 = none>
struct tiny
{
    typedef tiny type;

    typedef T0 t0;
    typedef T1 t1;
    typedef T2 t2;

    // ...
};



typedef mpl::vector3<int,long,char> S;

// pop the front element off S, unless it is empty
typedef mpl::eval_if<
    mpl::empty<S>
  , mpl::identity<S>
  , mpl::pop_front<S>
>::type r1;

// likewise
typedef mpl::eval_if<
    mpl::empty<S>
  , S                 // when invoked, S returns S
  , mpl::pop_front<S>
>::type r2;

BOOST_MPL_ASSERT((boost::is_same<r1,r2>));



template <class Tiny>
struct manipulate_tiny
{
    // what's T0?
    typedef typename Tiny::t0 t0;
};


template <class Tiny>
struct manipulate_tiny;

#if !defined(BOOST_MSVC) || BOOST_MSVC > 1300
// earlier versions don't support class template partial specialization
template <class T0, class T1, class T2>
struct manipulate_tiny<tiny<T0,T1,T2> >
{
    // T0 is known
};
#endif 
