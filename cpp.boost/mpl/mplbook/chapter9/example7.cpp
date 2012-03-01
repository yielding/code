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

        cl /nologo /Zm800 /EHsc -I- -I..\..\boost_1_32_0 -c /Fo%TEMP%\metaprogram-chapter9-example7.o example7.cpp

*/
#include <boost/mpl/vector.hpp>
#include <boost/type_traits/is_class.hpp>

#include <boost/mpl/placeholders.hpp>
#include <boost/mpl/assert.hpp>
#include <boost/static_assert.hpp>
namespace mpl = boost::mpl;
using namespace mpl::placeholders;

#include <iostream>
#include <typeinfo>
typedef mpl::vector<int&, long&, char*&> s;

template <bool> // handle integral constant wrappers
struct f_impl
{
    template <class T>
    static void print(T) { std::cout << T::value; }
};

template <>     // specialization for non-wrappers
struct f_impl<false>
{
    template <class T>
    static void print(T x) { std::cout << x; }
};

template <class T>
void f(T x)
{
    f_impl<boost::is_class<T>::value>::print(x);
};

int main() { f(42); return 0; }

