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

        cl /nologo /Zm800 /EHsc -I- -I..\..\boost_1_32_0 -c /Fo%TEMP%\metaprogram-chapter6-example1.o example1.cpp

*/
#include <vector>
#include <algorithm>

#include <boost/mpl/placeholders.hpp>
#include <boost/mpl/assert.hpp>
#include <boost/static_assert.hpp>
namespace mpl = boost::mpl;
using namespace mpl::placeholders;



  #include <functional>
  void f(std::vector<int>& v)
  {
      typedef std::vector<int>::iterator v_iter;
  

// "abstraction"
std::transform(
    v.begin(), v.end(), v.begin()
  , std::bind2nd(std::plus<int>(),42)
);

// handwritten loop
typedef std::vector<int>::iterator v_iter;
for (v_iter i = v.begin(), last = v.end(); i != last; ++i)
   *i += 42;

  }
  

