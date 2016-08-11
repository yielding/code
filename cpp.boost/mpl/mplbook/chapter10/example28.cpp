/*

    Copyright David Abrahams 2003-2004
    Copyright Aleksey Gurtovoy 2003-2004

    Distributed under the Boost Software License, Version 1.0. 
    (See accompanying file LICENSE_1_0.txt or copy at 
    http://www.boost.org/LICENSE_1_0.txt)
            
    This file was automatically extracted from the source of 
    "C++ Template Metaprogramming", by David Abrahams and 
    Aleksey Gurtovoy.

    It was NOT built successfully with Microsoft Visual C++ 6.0 SP6
    using the following command: 

        cl /nologo /Zm800 /EHsc -I- -I..\..\..\spirit_1_6_2 -I..\..\boost_1_32_0 -c /Fo%TEMP%\metaprogram-chapter10-example28.o example28.cpp

*/
#include <vector>
#include <iostream>
#include <boost/config.hpp>

#if defined(BOOST_MSVC) && BOOST_MSVC <= 1300
#error Even in Spirit 1.6.2, Phoenix doesn't support MSVC 6 or 7  
#endif 

    #include <boost/spirit/phoenix.hpp>
    using namespace phoenix;
    void f(std::vector<int> a)
    {
    

for_each(a.begin(), a.end(), 
         if_(arg1 % 2 != 0)
         [ 
            std::cout << arg1 
         ]
         .else_
         [
            std::cout << val('.')
         ]
);

    }
    

