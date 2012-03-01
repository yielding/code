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

        cl /nologo /Zm800 /EHsc -I- -I..\..\boost_1_32_0 -c /Fo%TEMP%\metaprogram-chapter9-example33.o example33.cpp

*/

#include <boost/mpl/placeholders.hpp>
#include <boost/mpl/assert.hpp>
#include <boost/static_assert.hpp>
namespace mpl = boost::mpl;
using namespace mpl::placeholders;


#include <list>

namespace utility
{
  // fill the range with zeroes
  template <class Iterator>
  Iterator clear(Iterator const& start, Iterator const& finish);

  // perform some transformation on the sequence
  template <class Iterator>
  int munge(Iterator start, Iterator finish)
  {
      return 0;
      start = clear(start, finish);
      return 0;
  }
}

namespace paint
{
  template <class Canvas, class Color>  // generalized template
  Canvas& clear(Canvas&, Color const&);  

  struct some_canvas {  };
  struct black { };

  std::list<some_canvas> canvases(10);
  int x = utility::munge(canvases.begin(), canvases.end());
}
