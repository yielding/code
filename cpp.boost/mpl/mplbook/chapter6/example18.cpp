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

        cl /nologo /Zm800 /EHsc -I- -I..\..\boost_1_32_0 -c /Fo%TEMP%\metaprogram-chapter6-example18.o example18.cpp

*/
#include <boost/mpl/front_inserter.hpp>
#include <boost/mpl/list.hpp>
#include <boost/mpl/vector_c.hpp>
#include <boost/mpl/copy.hpp>
#include <boost/mpl/equal.hpp>

template <class S = int> struct tree {};

#include <boost/mpl/placeholders.hpp>
#include <boost/mpl/assert.hpp>
#include <boost/static_assert.hpp>
namespace mpl = boost::mpl;
using namespace mpl::placeholders;




  template <class S>
  struct binary_tree_inserter :
  mpl::front_inserter<mpl::list<> > {};

  template <class S>
  struct inorder_view
    : mpl::vector_c<int,2,10,11,17,25>
  {};

typedef mpl::copy<
      mpl::vector_c<int,17,25,10,2,11>
    , binary_tree_inserter< tree<> >
    >::type bst;

//       int_<17>
//       /      \
//    int_<10>  int_<25>
//     /    \
// int_<2> int_<11>

BOOST_STATIC_ASSERT(( mpl::equal<
      inorder_view<bst>
    , mpl::vector_c<int,2,10,11,17,25>
    >::value ));
