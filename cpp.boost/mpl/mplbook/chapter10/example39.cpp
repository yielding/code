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

        cl /nologo /Zm800 /EHsc -I- -I..\..\..\spirit_1_6_2 -I..\..\boost_1_32_0 -c /Fo%TEMP%\metaprogram-chapter10-example39.o example39.cpp

*/


    #include <boost/spirit/core.hpp>
    #include <boost/spirit/attribute.hpp>
    using namespace boost::spirit;
    

struct vars : closure<vars, int>
{
    member1 value;
};
