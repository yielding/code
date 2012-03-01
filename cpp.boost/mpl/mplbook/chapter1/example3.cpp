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

        cl /nologo /Zm800 /EHsc -I- -I..\..\boost_1_32_0 -c /Fo%TEMP%\metaprogram-chapter1-example3.o example3.cpp

*/

template <unsigned long N> 
struct binary
{
    enum { value
         = binary<N/10>::value << 1   // prepend higher bits
         | N%10 };                    // to lowest bit
}; 

template <>                           // specialization
struct binary<0>                      // terminates recursion
{
    enum { value = 0 };
};

unsigned const one   =    binary<1>::value;
unsigned const three =   binary<11>::value;
unsigned const five  =  binary<101>::value;
unsigned const seven =  binary<111>::value;
unsigned const nine  = binary<1001>::value;

 #include <boost/static_assert.hpp>
 BOOST_STATIC_ASSERT(one == 1);
 BOOST_STATIC_ASSERT(three == 3);
 BOOST_STATIC_ASSERT(five == 5);
 BOOST_STATIC_ASSERT(seven == 7);
 BOOST_STATIC_ASSERT(nine == 9);

