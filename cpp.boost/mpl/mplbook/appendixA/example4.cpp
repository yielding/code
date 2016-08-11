/*

    Copyright David Abrahams 2003-2004
    Copyright Aleksey Gurtovoy 2003-2004

    Distributed under the Boost Software License, Version 1.0. 
    (See accompanying file LICENSE_1_0.txt or copy at 
    http://www.boost.org/LICENSE_1_0.txt)
            
    This file was automatically extracted from the source of 
    "C++ Template Metaprogramming", by David Abrahams and 
    Aleksey Gurtovoy.

    It is expected to FAIL to preprocess, but does not, with Microsoft
    Visual C++ 6.0 SP6 using the following command:

        cl /nologo /Zm800 /EHsc -I- -I..\..\boost_1_32_0 -E /Fo%TEMP%\metaprogram-appendixA-example4.o example4.cpp

*/

#include <boost/mpl/placeholders.hpp>
#include <boost/mpl/assert.hpp>
#include <boost/static_assert.hpp>
namespace mpl = boost::mpl;
using namespace mpl::placeholders;

struct none {};


#define FOO(X) X

FOO(std::pair<int, long>)                // two arguments
FOO({ int x = 1, y = 2; return x+y; })   // two arguments
