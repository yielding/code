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

        cl /nologo /Zm800 /EHsc -I- -I..\..\boost_1_32_0 -c /Fo%TEMP%\metaprogram-chapter6-example15.o example15.cpp

*/
#include <boost/mpl/vector_c.hpp>
#include <boost/mpl/list_c.hpp>
#include <boost/mpl/plus.hpp>
#include <boost/mpl/int.hpp>
#include <boost/mpl/transform.hpp>
#include <boost/mpl/equal.hpp>
#include <boost/mpl/equal_to.hpp>
#include <boost/mpl/back_inserter.hpp>
#include <boost/mpl/vector.hpp>
#include <boost/mpl/front_inserter.hpp>
#include <boost/mpl/list.hpp>

#include <boost/mpl/placeholders.hpp>
#include <boost/mpl/assert.hpp>
#include <boost/static_assert.hpp>
namespace mpl = boost::mpl;
using namespace mpl::placeholders;


// starting sequences
typedef mpl::vector_c<int, 1, 2, 3> v123;
typedef mpl::list_c<int, 1, 2, 3>   l123;

// transformation
typedef mpl::plus<_1,mpl::int_<5> > add5;

// using the default inserters
typedef mpl::transform<v123, add5>::type          v678;
typedef mpl::transform<l123, add5>::type          l678;
typedef mpl::reverse_transform<v123, add5>::type  v876;
typedef mpl::reverse_transform<l123, add5>::type  l876;

  typedef mpl::vector_c<int,6,7,8> _678;
  typedef mpl::vector_c<int,8,7,6> _876;




  BOOST_MPL_ASSERT((mpl::equal<v678,_678, mpl::equal_to<_,_> >));
  BOOST_MPL_ASSERT((mpl::equal<l678,_678, mpl::equal_to<_,_> >));
  BOOST_MPL_ASSERT((mpl::equal<v876,_876, mpl::equal_to<_,_> >));
  BOOST_MPL_ASSERT((mpl::equal<l876,_876, mpl::equal_to<_,_> >));
namespace fu
  {
      typedef mpl::vector_c<int,6,7,8> _678;
      typedef mpl::vector_c<int,8,7,6> _876;

// this inserter is equivalent to the default
typedef mpl::transform< 
    v123, add5, mpl::back_inserter<mpl::vector<> >
>::type                                            v678;

// also equivalent to the default
typedef mpl::reverse_transform<
    l123, add5, mpl::front_inserter<mpl::list<> >
>::type                                            l678;

// properties of input sequence don't affect the result
typedef mpl::reverse_transform<
    v123, add5, mpl::front_inserter<mpl::list<> >
>::type                                            l678;

  BOOST_MPL_ASSERT((mpl::equal<v678,_678, mpl::equal_to<_,_> >));
  BOOST_MPL_ASSERT((mpl::equal<l678,_678, mpl::equal_to<_,_> >));
  BOOST_MPL_ASSERT((mpl::equal<v876,_876, mpl::equal_to<_,_> >));
  BOOST_MPL_ASSERT((mpl::equal<l876,_876, mpl::equal_to<_,_> >));

  }

