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

        cl /nologo /Zm800 /EHsc -I- -I..\..\boost_1_32_0 -c /Fo%TEMP%\metaprogram-chapter6-example11.o example11.cpp

*/
#include <boost/mpl/fold.hpp>
#include <boost/mpl/clear.hpp>
#include <boost/mpl/push_front.hpp>
#include <boost/mpl/vector_c.hpp>
#include <boost/mpl/equal.hpp>
#include <boost/mpl/reverse.hpp>

#include <boost/mpl/placeholders.hpp>
#include <boost/mpl/assert.hpp>
#include <boost/static_assert.hpp>
namespace mpl = boost::mpl;
using namespace mpl::placeholders;


template <class Seq>
struct reverse
  : mpl::fold<
       Seq
     , typename mpl::clear<Seq>::type // initial type
     , mpl::push_front<_,_>           // binary operation
    >
{};

  typedef mpl::vector_c<int, 2, 3, 5, 7, 11> primes;
  BOOST_STATIC_ASSERT(( 
      mpl::equal<reverse<primes>::type
    , mpl::reverse<primes>::type>::value 
  ));

