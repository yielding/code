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

        cl /nologo /Zm800 /EHsc -I- -I..\..\boost_1_32_0 -c /Fo%TEMP%\metaprogram-chapter10-example21.o example21.cpp

*/


   template <class T,class U> struct some_class_template;
   template <class> struct slew_type_is;
   template <class> struct name_type_is;
   typedef

some_class_template<
    slew_type_is<float>        // slew_type = float
  , name_type_is<char const*>  // slew_type = char const*
>

test;

