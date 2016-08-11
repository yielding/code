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

        cl /nologo /Zm800 /EHsc -I- -I..\..\boost_1_32_0 -c /Fo%TEMP%\metaprogram-chapter2-example25.o example25.cpp

*/


 #include <iostream>
 template<class> int type_descriptor() { return 0; }
 void f() {
 

// prints "int"
std::cout << type_descriptor<int>(); 

// prints "char*"
std::cout << type_descriptor<char*>(); 

// prints "long const*&"
std::cout << type_descriptor<long const*&>(); 

}

