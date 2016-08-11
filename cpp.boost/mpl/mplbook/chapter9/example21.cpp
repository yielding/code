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

        cl /nologo /Zm800 /EHsc -I- -I..\..\boost_1_32_0 -c /Fo%TEMP%\metaprogram-chapter9-example21.o example21.cpp

*/

#include <boost/mpl/placeholders.hpp>
#include <boost/mpl/assert.hpp>
#include <boost/static_assert.hpp>
namespace mpl = boost::mpl;
using namespace mpl::placeholders;



typedef int pixel_map;

typedef float (*floatfunc)(float);

class screensaver
{
 public:
    explicit screensaver(floatfunc get_seed)
      : get_seed(get_seed)
    {}

    pixel_map next_screen()   // main algorithm
    {
        float center_pixel_brightness = 3.14;
        float seed = this->get_seed(center_pixel_brightness);
        return 0;
    }

 private:
    floatfunc get_seed;
    /* other members... */
};
