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

        cl /nologo /Zm800 /EHsc -I- -I..\..\boost_1_32_0 -c /Fo%TEMP%\metaprogram-chapter2-example20.o example20.cpp

*/

struct true_type {}; struct false_type {};

template <class T> struct type_traits // assume nothing
{ 
   typedef false_type has_trivial_default_constructor;
   typedef false_type has_trivial_copy_constructor;
   typedef false_type has_trivial_assignment_operator;
   typedef false_type has_trivial_destructor;
   typedef false_type is_POD_type;
};

template<> struct type_traits<char> // specialization for char
{ 
   typedef true_type has_trivial_default_constructor;
   typedef true_type has_trivial_copy_constructor;
   typedef true_type has_trivial_assignment_operator;
   typedef true_type has_trivial_destructor;
   typedef true_type is_POD_type;
};
/* more specializations follow... */
