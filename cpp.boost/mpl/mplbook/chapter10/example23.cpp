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

        cl /nologo /Zm800 /EHsc -I- -I..\..\boost_1_32_0 -c /Fo%TEMP%\metaprogram-chapter10-example23.o example23.cpp

*/
#include <boost/config.hpp>
#include <string>
#include <algorithm>


#include <functional>


struct Visitor { void visit(int) const; };

struct X { float foo(float) const; } obj;

    template <class T> int test(T);
    using namespace std;
    namespace ext = std;
    using namespace ext;
    double const pi = 3.14;

int test1 = test(
    bind1st(std::plus<string>(), string("hello, "))
);

#if defined(BOOST_MSVC)
#error MSVC doesn't have compose2, and in version 6 mem_fun_ref doesn't work properly
#endif

int test2 = test(
compose2(
    std::multiplies<float>()
  , bind2nd(std::minus<float>(), 1)
  , identity<float>())

);

int test3 = test(
mem_fun_ref(&X::foo)(obj, pi)

);
            void test4(int*first,int*last,Visitor v)
            { 
                 std::for_each(
    first
  , last
  , bind1st(mem_fun_ref(&Visitor::visit), v));

}

