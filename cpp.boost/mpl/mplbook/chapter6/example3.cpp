/*

    Copyright David Abrahams 2003-2004
    Copyright Aleksey Gurtovoy 2003-2004

    Distributed under the Boost Software License, Version 1.0. 
    (See accompanying file LICENSE_1_0.txt or copy at 
    http://www.boost.org/LICENSE_1_0.txt)
            
    This file was automatically extracted from the source of 
    "C++ Template Metaprogramming", by David Abrahams and 
    Aleksey Gurtovoy.

    It was NOT built successfully with Microsoft Visual C++ 6.0 SP6
    using the following command: 

        cl /nologo /Zm800 /EHsc -I- -I..\..\boost_1_32_0 -c /Fo%TEMP%\metaprogram-chapter6-example3.o example3.cpp

*/
#include <vector>
#include <algorithm>

#include <boost/mpl/placeholders.hpp>
#include <boost/mpl/assert.hpp>
#include <boost/static_assert.hpp>
namespace mpl = boost::mpl;
using namespace mpl::placeholders;


#if defined(BOOST_MSVC) && BOOST_MSVC <= 1300
# error The Boost.Lambda library doesn't support MSVC 6 and 7; try 7.1 or later
#endif

   #include <boost/lambda/lambda.hpp>
   namespace ll
   {
       using boost::lambda::_1;

  #include <functional>
  void f(std::vector<int>& v)
  {
      typedef std::vector<int>::iterator v_iter;
  

std::for_each(v.begin(), v.end(), _1 += 42);

  }
  
}

