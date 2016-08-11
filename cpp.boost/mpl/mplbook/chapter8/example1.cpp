/*

    Copyright David Abrahams 2003-2004
    Copyright Aleksey Gurtovoy 2003-2004

    Distributed under the Boost Software License, Version 1.0. 
    (See accompanying file LICENSE_1_0.txt or copy at 
    http://www.boost.org/LICENSE_1_0.txt)
            
    This file was automatically extracted from the source of 
    "C++ Template Metaprogramming", by David Abrahams and 
    Aleksey Gurtovoy.

    It FAILS to compile, as expected, with Microsoft Visual C++ 6.0 SP6
    using the following command: 

        cl /nologo /Zm800 /EHsc -I- -I..\..\boost_1_32_0 -c /Fo%TEMP%\metaprogram-chapter8-example1.o example1.cpp

*/
#include <boost/mpl/int.hpp>

#include <boost/mpl/placeholders.hpp>
#include <boost/mpl/assert.hpp>
#include <boost/static_assert.hpp>
namespace mpl = boost::mpl;
using namespace mpl::placeholders;


struct nil {};                     // the end of every list

template <class H, class T = nil>  // a list node, e.g:
struct node                        // node<X,node<Y,node<Z> > >
{
    typedef H head; typedef T tail;
};

template <class S>
struct total_size
{
    typedef typename total_size<   // total size of S::tail
        typename S::tail            
    >::type tail_size;             // line 17

    typedef boost::mpl::int_<      // add size of S::head
        sizeof(S::head)
        + tail_size::value         // line 22
    > type;
};


typedef total_size<
    node<long, node<int, node<char> > >
>::type x;                          // line 27
