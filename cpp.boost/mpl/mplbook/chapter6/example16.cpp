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

        cl /nologo /Zm800 /EHsc -I- -I..\..\boost_1_32_0 -c /Fo%TEMP%\metaprogram-chapter6-example16.o example16.cpp

*/
#include <boost/mpl/equal_to.hpp>
#include <boost/mpl/shift_right.hpp>
#include <boost/mpl/int.hpp>
#include <boost/mpl/vector_c.hpp>
#include <boost/mpl/unique.hpp>
#include <boost/mpl/equal.hpp>

#include <boost/mpl/placeholders.hpp>
#include <boost/mpl/assert.hpp>
#include <boost/static_assert.hpp>
namespace mpl = boost::mpl;
using namespace mpl::placeholders;


typedef mpl::equal_to<           
    mpl::shift_right<_1, mpl::int_<1> >
  , mpl::shift_right<_2, mpl::int_<1> >
> same_except_last_bit;                     // Predicate

typedef mpl::vector_c<int, 0,1,2,3,4,5> v;

typedef mpl::unique<
     v, same_except_last_bit
>::type                      v024;           // 0, 2, 4

typedef mpl::reverse_unique<
     v, same_except_last_bit
>::type                      v531;           // 5, 3, 1

  BOOST_MPL_ASSERT((
      mpl::equal<
          v024, mpl::vector_c<int,0,2,4>, mpl::equal_to<_,_>
      >));
  BOOST_MPL_ASSERT((
      mpl::equal<
          v531, mpl::vector_c<int,5,3,1>, mpl::equal_to<_,_>
      >));
  

