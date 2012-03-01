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

        cl /nologo /Zm800 /EHsc -I- -I..\..\boost_1_32_0 -c /Fo%TEMP%\metaprogram-chapter9-example13.o example13.cpp

*/
#include <boost/mpl/vector.hpp>

#include <boost/mpl/placeholders.hpp>
#include <boost/mpl/assert.hpp>
#include <boost/static_assert.hpp>
namespace mpl = boost::mpl;
using namespace mpl::placeholders;

#include <iostream>
#include <typeinfo>
typedef mpl::vector<int&, long&, char*&> s;
#include <functional>

template <class R, class F, class G>
class compose_fg : F   // if empty, F may overlap with g
{                      
 public:
    typedef R result_type;
 
    compose_fg(F const& f, G const& g)
      : F(f), g(g)    // initialize base with f
    {}

    template <class T>
    R operator()(T const& x) const
    {
        F const& f = *this;   // retrieve F subobject
        return f(g(x));
    }
 private:
    G g;
};



typedef 

compose_fg<float,std::negate<float>,float(*)(float)>

    fn; 
    float foo(float x) { return x; }
    float x = fn(std::negate<float>(),&foo)(42);

