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

        cl /nologo /Zm800 /EHsc -I- -I..\..\boost_1_32_0 -c /Fo%TEMP%\metaprogram-chapter9-example24.o example24.cpp

*/
#include <memory>

#include <boost/mpl/placeholders.hpp>
#include <boost/mpl/assert.hpp>
#include <boost/static_assert.hpp>
namespace mpl = boost::mpl;
using namespace mpl::placeholders;


class screensaver
{
 private:
    struct customization
    {
        virtual ~customization() {}
        virtual float operator()(float) const = 0;
    };

    template <class F>                   // a wrapper for an F
    struct wrapper : customization
    {
        explicit wrapper(F f)         
          : f(f) {}                      // store an F

        float operator()(float x) const
        {
            return this->f(x);           // delegate to stored F
        }

     private:
         F f;
    };

 public:
    template <class F>
    explicit screensaver(F const& f) 
      : get_seed(new wrapper<F>(f))
    {}
    // ...
 private:
    std::auto_ptr<customization> get_seed;
    // ...
};
