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

        cl /nologo /Zm800 /EHsc -I- -I..\..\boost_1_32_0 -c /Fo%TEMP%\metaprogram-chapter9-example17.o example17.cpp

*/
#include <boost/mpl/vector.hpp>
#include <boost/type_traits/is_same.hpp>

#include <boost/mpl/placeholders.hpp>
#include <boost/mpl/assert.hpp>
#include <boost/static_assert.hpp>
namespace mpl = boost::mpl;
using namespace mpl::placeholders;

#include <iostream>
#include <typeinfo>
typedef mpl::vector<int&, long&, char*&> s;
#include <functional>

    template <class T, class U>
    int test_type_of(U const&)
    {
        BOOST_MPL_ASSERT((boost::is_same<T,U>));
        return 0;
    }
    

template <class R, class F, F f, class G, G g>
struct compose_fg2
{
    typedef R result_type;
 
    template <class T>
    R operator()(T const& x) const
    {
        return f(g(x));
    }
};



#include <cmath>
using namespace std; // in MSVC6, sin and log are not in std.
float sin_squared(float x) { return sin(sin(x)); }
#undef log2 // g++'s <cmath> seems to define this symbol

#include <functional>
#include <algorithm>

float input[5] = {0.0, 0.1, 0.2, 0.3, 0.4};
float output[5];

inline float log2(float x) { return log(x)/log(2.0f); }

typedef float (*floatfun)(float);

float* ignored = std::transform(
    input, input+5, output
  , compose_fg2<float, floatfun,sin_squared, floatfun,log2>()
);
