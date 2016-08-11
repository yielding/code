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

        cl /nologo /Zm800 /EHsc -I- -I..\..\boost_1_32_0 -c /Fo%TEMP%\metaprogram-appendixC-example11.o example11.cpp

*/

#include <boost/mpl/placeholders.hpp>
#include <boost/mpl/assert.hpp>
#include <boost/static_assert.hpp>
namespace mpl = boost::mpl;
using namespace mpl::placeholders;



template <class T, class Base> class node; typedef

node<int, node<long, node<char, node<void, void> > > >

 x;




typedef

// 1   2   3 <== Depths           Nodes
   node<                       // #1
       node<                   // #2
           node<void,void>     // #3
         , node<int,void>      // #4
       >
     , node<void, void>        // #3 again
   >

y;

