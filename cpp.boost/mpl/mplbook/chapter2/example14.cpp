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

        cl /nologo /Zm800 /EHsc -I- -I..\..\boost_1_32_0 -c /Fo%TEMP%\metaprogram-chapter2-example14.o example14.cpp

*/


#include <iterator>
using namespace std;

 // Generalized (slow) version
 template <class ForwardIterator1, class ForwardIterator2>
 void iter_swap(ForwardIterator1 i1, ForwardIterator2 i2)
 {
     typename 
       iterator_traits<ForwardIterator1>::value_type 
     tmp = *i1; 
     *i1 = *i2;
     *i2 = tmp;
 }

// A better match when the two iterators are the same type
template <class ForwardIterator>
void iter_swap(ForwardIterator i1, ForwardIterator i2)
{
    std::swap(*i1, *i2); // sometimes faster
}
