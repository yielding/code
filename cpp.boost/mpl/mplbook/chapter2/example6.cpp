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

        cl /nologo /Zm800 /EHsc -I- -I..\..\boost_1_32_0 -c /Fo%TEMP%\metaprogram-chapter2-example6.o example6.cpp

*/

#include <boost/config.hpp> // for BOOST_MSVC

template <class T> struct iterator_traits;

#if !defined(BOOST_MSVC) || BOOST_MSVC > 1300  // earlier versions don't support partial specialization.
template <class T> 
struct iterator_traits<T*> {
    typedef T value_type;
    /* ...four more typedefs */
};
#endif 
