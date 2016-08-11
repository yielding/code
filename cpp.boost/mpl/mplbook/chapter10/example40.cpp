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

        cl /nologo /Zm800 /EHsc -I- -I..\..\boost_1_32_0 -c /Fo%TEMP%\metaprogram-chapter10-example40.o example40.cpp

*/


template <class A, class B> struct alternative {};
template <class A, class B> struct sequence {};
template <class A, class B> struct rule {};
typedef int Tokenizer;
typedef int ch_p;
struct vars { typedef int context_t; };

alternative<
    rule<Tokenizer, vars::context_t>  // for integer rule
  , rule<Tokenizer, vars::context_t>  // for group rule
>

x;



sequence<
    sequence<
        ch_p                              // for '(' parser
      , rule<Tokenizer, vars::context_t>  // expression rule
    >
  , ch_p                                  // for ')' parser
>

y;

