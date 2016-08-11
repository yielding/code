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

        cl /nologo /Zm800 /EHsc -I- -I..\..\boost_1_32_0 -c /Fo%TEMP%\metaprogram-chapter7-example8.o example8.cpp

*/
#include <boost/mpl/transform_view.hpp>
#include <boost/mpl/at.hpp>
#include <boost/mpl/list_c.hpp>
#include <boost/mpl/vector_c.hpp>
#include <boost/mpl/equal.hpp>

#include <boost/mpl/placeholders.hpp>
#include <boost/mpl/assert.hpp>
#include <boost/static_assert.hpp>
namespace mpl = boost::mpl;
using namespace mpl::placeholders;



   template <class Indices, class Elements>
   struct permutation_view
     : mpl::transform_view<
           Indices
         , mpl::at<Elements,_>
       >
   {}; typedef

permutation_view< 
    mpl::list_c<int,2,1,3,0,2>     // indices
  , mpl::vector_c<int,11,22,33,44> // elements
>

permuted;
   BOOST_MPL_ASSERT((
     mpl::equal<permuted,mpl::vector_c<int,33,22,44,11,33> >
   ));

