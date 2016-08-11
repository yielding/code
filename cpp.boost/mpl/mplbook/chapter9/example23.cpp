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

        cl /nologo /Zm800 /EHsc -I- -I..\..\boost_1_32_0 -c /Fo%TEMP%\metaprogram-chapter9-example23.o example23.cpp

*/
#include <memory>
#include <vector>

#include <boost/mpl/placeholders.hpp>
#include <boost/mpl/assert.hpp>
#include <boost/static_assert.hpp>
namespace mpl = boost::mpl;
using namespace mpl::placeholders;



typedef int pixel_map;

class screensaver
{
 public:
    struct customization
    {
        virtual ~customization() {}
        virtual float operator()(float) const = 0;
    };

    explicit screensaver(std::auto_ptr<customization> c)
      : get_seed(c)
    {}

    pixel_map next_screen()
    {
        float center_pixel_brightness = 3.14;
        float seed = (*this->get_seed)(center_pixel_brightness);
        return 0;
    }

 private:
    std::auto_ptr<customization> get_seed;
    // ...
};


struct hypnotic : screensaver::customization
{
    float operator()(float) const 
    {
        return 3.14;
    }
    std::vector<int> state;
};



typedef float (*floatfunc)(float);

struct funwrapper : screensaver::customization
{
    funwrapper(floatfunc pf)
      : pf(pf) {}

    float operator()(float x) const
    {
        return this->pf(x);
    }

    floatfunc pf; // stored function pointer
};
