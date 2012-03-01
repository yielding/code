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

        cl /nologo /Zm800 /EHsc -I- -I..\..\boost_1_32_0 -c /Fo%TEMP%\metaprogram-appendixA-example23.o example23.cpp

*/

#include <boost/mpl/placeholders.hpp>
#include <boost/mpl/assert.hpp>
#include <boost/static_assert.hpp>
namespace mpl = boost::mpl;
using namespace mpl::placeholders;

struct none {};

#define TUPLE3     (f(12), a + 1, foo)



        #include <boost/preprocessor/tuple.hpp>
        int const foo = 0;
        int const x =

#define FIRST_OF_THREE(a1,a2,a3)    a1
#define SECOND_OF_THREE(a1,a2,a3)   a2
#define THIRD_OF_THREE(a1,a2,a3)    a3

// uses tuple as an argument list
# define SELECT(selector, tuple)    selector tuple

SELECT(THIRD_OF_THREE, TUPLE3)   // foo

;

