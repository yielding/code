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

        cl /nologo /Zm800 /EHsc -I- -I..\..\boost_1_32_0  /Fe%TEMP%\metaprogram-appendixC-example2.exe example2.cpp

*/
#include <iostream>

#include <boost/mpl/placeholders.hpp>
#include <boost/mpl/assert.hpp>
#include <boost/static_assert.hpp>
#include <boost/mpl/aux_/nttp_decl.hpp>
namespace mpl = boost::mpl;
using namespace mpl::placeholders;


unsigned fibonacci(BOOST_MPL_AUX_NTTP_DECL(unsigned, n))
{
    if (n < 2) 
        return n;
    else 
        return fibonacci(n - 1) + fibonacci(n - 2);
}

int main() { std::cout << "(" << fibonacci(6) << ")"; return 0; }

