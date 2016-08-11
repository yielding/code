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

        cl /nologo /Zm800 /EHsc -I- -I..\..\boost_1_32_0 -I..\..\..\Blitz++-0.7 -c /Fo%TEMP%\metaprogram-chapter10-example16.o example16.cpp

*/

#error Blitz++ doesn't support MSVC6/7

    #define __restrict__
    namespace {
    double j0(double);
    double j1(double);
    double y0(double);
    double y1(double);
    }
    #include <blitz/array.h>
    using namespace blitz;
    


void f(Array<float,2>& B) {

Array<float,2> A(3,3);

A = 1, 2, 3,
    4, 5, 6,
    7, 8, 9;


// add the first two rows and columns of A to B
B += A(Range(0,2), Range(0,2));

}

