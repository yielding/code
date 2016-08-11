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

        cl /nologo /Zm800 /EHsc -I- -I..\..\boost_1_32_0 -c /Fo%TEMP%\metaprogram-chapter6-example9.o example9.cpp

*/
#include <boost/mpl/copy.hpp>
#include <boost/mpl/list.hpp>
#include <boost/mpl/back_inserter.hpp>
#include <boost/mpl/vector.hpp>
#include <boost/mpl/push_back.hpp>
#include <boost/type_traits/is_same.hpp>

#include <boost/mpl/placeholders.hpp>
#include <boost/mpl/assert.hpp>
#include <boost/static_assert.hpp>
namespace mpl = boost::mpl;
using namespace mpl::placeholders;



typedef int A; typedef char B; typedef short C;

typedef mpl::copy<
    mpl::list<A, B, C>
  , mpl::back_inserter<mpl::vector<> >
>::type result_vec;


typedef 

  mpl::push_back<           // >----------------+
                            //                  |
      mpl::push_back<       // >--------------+ |
                            //                | |
          mpl::push_back<   // >------------+ | |
              mpl::vector<> //              | | |
            , A             //              | | |
          >::type           // first step <-+ | |
        , B                 //                | |
      >::type               // second step <--+ |
    , C                     //                  |
  >::type                   // third step <-----+

result_vec2;

BOOST_STATIC_ASSERT(( boost::is_same<result_vec,result_vec2>::value ));

