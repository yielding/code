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

        cl /nologo /Zm800 /EHsc -I- -I..\..\boost_1_32_0 -c /Fo%TEMP%\metaprogram-chapter2-example16.o example16.cpp

*/


typedef unsigned size_t;

struct proxy
{
   proxy& operator=(bool x)
   {
       if (x) 
           bytes[pos/8] |= (1u << (pos%8));
       else
           bytes[pos/8] &= ~(1u << (pos%8));
       return *this;
   }

   operator bool() const
   {
       return bytes[pos/8] & (1u << (pos%8));
   }

   unsigned char* bytes;
   size_t pos;
};

struct bit_iterator
{
   typedef bool value_type;
   typedef proxy reference;
   /* more typedefs... */

   proxy operator*() const;
   /* more operations... */
};
