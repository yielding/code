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

        cl /nologo /Zm800 /EHsc -I- -I..\..\boost_1_32_0 -c /Fo%TEMP%\metaprogram-chapter9-example10.o example10.cpp

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

template <class R, class F, class G>
class compose_fg
{
 public:
    compose_fg(F const& f, G const& g)
      : f(f), g(g)
    {}

    template <class T>
    R operator()(T const& x) const
    {
        return f(g(x));
    }

 private:
    F f;
    G g; 
};


#include <functional>
#include <algorithm>
#include <cmath>

float input[5] = {0.0, 0.1, 0.2, 0.3, 0.4};
float output[5];                          

using namespace std; // in MSVC6, sin is not in std.
float sin_squared(float x) { return sin(sin(x)); }

float* ignored = std::transform(
    input, input+5, output
  , compose_fg<float,std::negate<float>,float(*)(float)>(
       std::negate<float>(), &sin_squared
    )
);


template <class R, class F, class G>
compose_fg<R,F,G> compose(F const& f, G const& g)
{
    return compose_fg<R,F,G>(f,g);
}



namespace n2 {

float* ignored = std::transform(
    input, input+5, output 
  , compose<float>(std::negate<float>(), &sin_squared)
);

}

