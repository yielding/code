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

        cl /nologo /Zm800 /EHsc -I- -I..\..\boost_1_32_0 -c /Fo%TEMP%\metaprogram-chapter7-example7.o example7.cpp

*/
#include <boost/mpl/joint_view.hpp>
#include <boost/mpl/iterator_range.hpp>
#include <boost/mpl/begin_end.hpp>
#include <boost/mpl/vector_c.hpp>
#include <boost/mpl/advance.hpp>
#include <boost/mpl/equal.hpp>
#include <boost/mpl/range_c.hpp>

#include <boost/mpl/placeholders.hpp>
#include <boost/mpl/assert.hpp>
#include <boost/static_assert.hpp>
namespace mpl = boost::mpl;
using namespace mpl::placeholders;



    template <class S, class Pos>
    struct rotate_view
      : mpl::joint_view<
            mpl::iterator_range<
                Pos
              , typename mpl::end<S>::type
            >
          , mpl::iterator_range<
                typename mpl::begin<S>::type
              , Pos
            >
        >
    {};

typedef mpl::vector_c<int,5,6,7,8,9,0,1,2,3,4> v;
typedef rotate_view<
    v
  , mpl::advance_c<mpl::begin<v>::type,5>::type
> view;
    
BOOST_STATIC_ASSERT(( mpl::equal<
    view
  , mpl::range_c<int,0,10>
>::value ));
