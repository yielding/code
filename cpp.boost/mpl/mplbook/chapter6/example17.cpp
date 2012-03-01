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

        cl /nologo /Zm800 /EHsc -I- -I..\..\boost_1_32_0 -c /Fo%TEMP%\metaprogram-chapter6-example17.o example17.cpp

*/
#include <boost/mpl/min_element.hpp>
#include <boost/mpl/less.hpp>
#include <boost/mpl/sizeof.hpp>
#include <boost/type_traits/is_same.hpp>
#include <boost/mpl/vector.hpp>

#include <boost/mpl/placeholders.hpp>
#include <boost/mpl/assert.hpp>
#include <boost/static_assert.hpp>
namespace mpl = boost::mpl;
using namespace mpl::placeholders;



  template <class S>
  struct smallest
  {
    typedef typename mpl::deref<
        typename mpl::min_element<
            S, mpl::less<mpl::sizeof_<_1>,mpl::sizeof_<_2> >
         >::type
    >::type type;
  };

#if defined(BOOST_MSVC) && BOOST_MSVC <= 1300
template <> struct smallest<int> { typedef int type; };
#endif


BOOST_STATIC_ASSERT((
    boost::is_same<
        smallest< mpl::vector<int[2], char, double&> >::type
      , char
    >::value
));
