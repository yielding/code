/*

    Copyright David Abrahams 2003-2004
    Copyright Aleksey Gurtovoy 2003-2004

    Distributed under the Boost Software License, Version 1.0. 
    (See accompanying file LICENSE_1_0.txt or copy at 
    http://www.boost.org/LICENSE_1_0.txt)
            
    This file was automatically extracted from the source of 
    "C++ Template Metaprogramming", by David Abrahams and 
    Aleksey Gurtovoy.

    It was NOT built successfully with Microsoft Visual C++ 6.0 SP6
    using the following command: 

        cl /nologo /Zm800 /EHsc -I- -I..\..\boost_1_32_0 -c /Fo%TEMP%\metaprogram-chapter9-example37.o example37.cpp

*/
#include <boost/mpl/bool.hpp>

    #include <boost/mpl/placeholders.hpp>
    #include <boost/mpl/assert.hpp>
    #include <boost/static_assert.hpp>
    namespace mpl = boost::mpl;
    using namespace mpl::placeholders;
    
typedef char yes;      // sizeof(yes) == 1
typedef char (&no)[2]; // sizeof(no)  == 2

#    if defined(BOOST_MSVC) && BOOST_MSVC <= 1300
#     error This kind of SFINAE doesn't work with MSVC 6 or 7
#     error we use other techniques to detect classes on those compilers.
#    endif 

template <class T>
struct is_class_or_union
{
    // SFINAE eliminates this when the type of arg is invalid
    template <class U> 
    static yes tester(int U::*arg);
    
    // overload resolution prefers anything at all over "..."
    template <class U> 
    static no tester(...);
    
    // see which overload is chosen when U == T
    static bool const value
       = sizeof(is_class_or_union::tester<T>(0)) == sizeof(yes);

    typedef mpl::bool_<value> type;
};

struct X{};
BOOST_STATIC_ASSERT(is_class_or_union<X>::value);
BOOST_STATIC_ASSERT(!is_class_or_union<int>::value);
