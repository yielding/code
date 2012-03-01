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

        cl /nologo /Zm800 /EHsc -I- -I..\..\boost_1_32_0 -c /Fo%TEMP%\metaprogram-chapter6-example4.o example4.cpp

*/
#include <boost/mpl/deref.hpp>
#include <boost/mpl/max_element.hpp>
#include <boost/mpl/replace.hpp>
#include <boost/mpl/less.hpp>
#include <boost/mpl/sizeof.hpp>

#include <boost/mpl/placeholders.hpp>
#include <boost/mpl/assert.hpp>
#include <boost/static_assert.hpp>
namespace mpl = boost::mpl;
using namespace mpl::placeholders;


// Given a nonempty sequence Seq, returns the largest type in an
// identical sequence where all instances of float have been
// replaced by double.  
template <class Seq>
struct biggest_float_as_double
  : mpl::deref<
        typename mpl::max_element<
           typename mpl::replace<
               Seq
             , float
             , double
           >::type
         , mpl::less<mpl::sizeof_<_1>, mpl::sizeof_<_2> >
       >::type
   >
{};
