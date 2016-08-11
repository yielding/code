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

        cl /nologo /Zm800 /EHsc -I- -I..\..\boost_1_32_0 -c /Fo%TEMP%\metaprogram-appendixC-example10.o example10.cpp

*/
#include <boost/type_traits/is_same.hpp>
#include <boost/mpl/eval_if.hpp>
#include <boost/mpl/next.hpp>
#include <boost/mpl/deref.hpp>
#include <boost/mpl/identity.hpp>

#include <boost/mpl/placeholders.hpp>
#include <boost/mpl/assert.hpp>
#include <boost/static_assert.hpp>
namespace mpl = boost::mpl;
using namespace mpl::placeholders;



  namespace my
  {
      using boost::is_same;
      using namespace boost::mpl;

namespace boost { namespace mpl {

  template <class First, class Last, class T>
  struct find_impl;

  // find_impl on the tail of the sequence
  template <class First, class Last, class T>
  struct find_impl_tail
    : find_impl<
          typename next<First>::type
        , Last
        , T
      >
  {};

  // true if First points at T
  template <class First, class T>
  struct deref_is_same
    : is_same<typename deref<First>::type,T>
  {};

  template <class First, class Last, class T>
  struct find_impl
    : eval_if<
          is_same<First,Last>     // Terminating case
        , ::boost::mpl::identity<Last>
        , eval_if<
              deref_is_same<First,T>  // found?
            , ::boost::mpl::identity<First>                    
            , find_impl_tail<First,Last,T>  // search on the tail
          >
      >
  {};

}}

}

