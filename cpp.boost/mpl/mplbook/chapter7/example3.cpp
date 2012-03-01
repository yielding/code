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

        cl /nologo /Zm800 /EHsc -I- -I..\..\boost_1_32_0 -c /Fo%TEMP%\metaprogram-chapter7-example3.o example3.cpp

*/
#include <boost/mpl/range_c.hpp>
#include <boost/mpl/transform_view.hpp>
#include <boost/mpl/zip_view.hpp>
#include <boost/mpl/vector.hpp>
#include <boost/mpl/plus.hpp>
#include <boost/mpl/at.hpp>
#include <boost/mpl/int.hpp>
#include <boost/mpl/equal.hpp>
#include <boost/mpl/vector_c.hpp>
#include <boost/mpl/equal_to.hpp>

#include <boost/mpl/placeholders.hpp>
#include <boost/mpl/assert.hpp>
#include <boost/static_assert.hpp>
namespace mpl = boost::mpl;
using namespace mpl::placeholders;



       typedef mpl::range_c<int,0,4> V1;
       typedef mpl::range_c<int,3,7> V2;
       typedef mpl::range_c<int,6,10> V3;
       typedef 

mpl::transform_view<
    mpl::zip_view<mpl::vector<V1,V2,V3> >
  , mpl::plus<
        mpl::at<_, mpl::int_<0> >
      , mpl::at<_, mpl::int_<1> >
      , mpl::at<_, mpl::int_<2> >
    >
>

result;
       BOOST_MPL_ASSERT((
         mpl::equal<
            result, mpl::vector_c<int,9,12,15,18>, mpl::equal_to<_,_>
         >));
       

