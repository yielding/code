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

        cl /nologo /Zm800 /EHsc -I- -I..\..\boost_1_32_0 -c /Fo%TEMP%\metaprogram-chapter6-example12.o example12.cpp

*/
#include <boost/mpl/vector_c.hpp>
#include <boost/mpl/int.hpp>
#include <boost/mpl/less.hpp>
#include <boost/mpl/lower_bound.hpp>
#include <boost/mpl/upper_bound.hpp>
#include <boost/mpl/max_element.hpp>
#include <boost/mpl/begin_end.hpp>
#include <boost/mpl/apply.hpp>
#include <boost/mpl/deref.hpp>
#include <boost/mpl/min_element.hpp>

#include <boost/mpl/placeholders.hpp>
#include <boost/mpl/assert.hpp>
#include <boost/static_assert.hpp>
namespace mpl = boost::mpl;
using namespace mpl::placeholders;



  typedef mpl::vector_c<int, 2, 3, 5, 7> seq;
  typedef mpl::int_<4> T;
  typedef mpl::less<_,_> compare;
  


typedef 

mpl::lower_bound<
    seq, T
  , compare
>

::type t0;




typedef 

mpl::upper_bound<
    seq, T
  , compare
>

::type t1;




typedef 

mpl::max_element<
    seq
  , compare
>

::type t2;




          namespace n3 {
              typedef t2 i;
              typedef mpl::begin<seq>::type j;
              enum { test =
              

mpl::apply2<
    compare
  , mpl::deref<i>::type
  , mpl::deref<j>::type
>::type::value == false

                   };
              BOOST_STATIC_ASSERT(test);
          }
          




typedef 

mpl::min_element<
    seq
  , compare
>

::type t4;




          namespace n5 {
              typedef t4 i;
              typedef mpl::begin<seq>::type j;
              enum { test =
              

mpl::apply2<
    compare
  , mpl::deref<j>::type
  , mpl::deref<i>::type
>::type::value == false

                   };
              BOOST_STATIC_ASSERT(test);
          }
          

