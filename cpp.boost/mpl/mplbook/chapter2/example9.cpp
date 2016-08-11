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

        cl /nologo /Zm800 /EHsc -I- -I..\..\boost_1_32_0 -c /Fo%TEMP%\metaprogram-chapter2-example9.o example9.cpp

*/

template <class X, class UnaryOp1, class UnaryOp2>
X apply_fg(X x, UnaryOp1 f, UnaryOp2 g)
{
    return f(g(x));
}


#include <functional>
float log2(float);

int a = apply_fg(5.0f,  std::negate<float>(), log2);
int b = apply_fg(3.14f, log2, std::negate<float>());
