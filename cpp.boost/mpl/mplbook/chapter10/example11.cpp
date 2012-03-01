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

        cl /nologo /Zm800 /EHsc -I- -I..\..\boost_1_32_0 -c /Fo%TEMP%\metaprogram-chapter10-example11.o example11.cpp

*/


  #include <boost/array.hpp>
  struct Array
    : boost::array<boost::array<float,3>,3>
  {};

Array operator+(Array const& a, Array const& b)
{
     std::size_t const n = a.size();
     Array result;

     for (std::size_t row = 0; row != n; ++row)
         for (std::size_t col = 0; col != n; ++col)
             result[row][col] = a[row][col] + b[row][col];

     return result;
}



  Array f(Array a, Array b, Array c) 
  { 
      Array
  x, &result = x;
  std::size_t const n = a.size();

for (std::size_t row = 0; row != n; ++row)
    for (std::size_t col = 0; col != n; ++col)
        result[row][col] = a[row][col] + b[row][col] + c[row][col];

      return x; 
  }

