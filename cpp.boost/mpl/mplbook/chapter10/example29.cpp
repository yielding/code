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

        cl /nologo /Zm800 /EHsc -I- -I..\..\boost_1_32_0 -I..\..\..\FC++.1.5 -c /Fo%TEMP%\metaprogram-chapter10-example29.o example29.cpp

*/
#include <cstddef>

#include <boost/config.hpp>

#if defined(BOOST_MSVC) && BOOST_MSVC <= 1300
#error FC++ doesn't support MSVC 6 or 7  
#endif 
              using std::ptrdiff_t;
              using std::size_t;
              #define BOOST_FCPP_ENABLE_LAMBDA
              #include "fcpp/prelude.hpp"
              #include "fcpp/lambda.hpp"
              using namespace boost::fcpp;
              lambda_var<3> Y;
              struct gee { 
                  template <class T> void operator=(T); 
              } g;
              


void f() {

lambda_var<1> X;
lambda_var<2> Fun;
   
g = lambda(Fun,X)[ Fun[Fun[X]] ]   // g(fun,x) -> fun(fun(x))

;}

