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

        cl /nologo /Zm800 /EHsc -I- -I..\..\boost_1_32_0  /Fe%TEMP%\metaprogram-chapter10-example38.exe example38.cpp

*/

#include <iostream>

namespace foo
{
  int x = 76;

  int g()
  {
     return x + 1;      // refers to foo::x
  }
}

int main()
{ 
   int x = 42;
   std::cout << foo::g();    // prints 77
}
