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

        cl /nologo /Zm800 /EHsc -I- -I..\..\boost_1_32_0 -c /Fo%TEMP%\metaprogram-chapter10-example22.o example22.cpp

*/
#include <string>
#include <iostream>
#include <algorithm>
#include <functional>

#include <boost/bind.hpp>
using boost::bind;
  double const pi = 3.14;

std::string f(std::string, std::string);
  std::string hiworld =

      boost::bind<std::string>(
          std::plus<std::string>()
        , "hello, ", _1)
      ("world");




void test2() {

    std:: cout << boost::bind<std::string>(                     // prints "hello, world"
                  std::plus<std::string>()
                , "hello, ", _1
              )("world");  

}




float test3 = 

boost::bind<float>(
    std::multiplies<float>()
  , _1
  , boost::bind<float>(std::minus<float>(), _1, 0.5))

(pi);



struct X { float foo(float) const; } obj;



float test4 = 

boost::bind(&X::foo, _1, _2)(obj, pi)

;




struct Visitor { void visit(int) const; };        
    void test5(int* first, int* last, Visitor v)
    {
    

std::for_each(first, last, boost::bind(&Visitor::visit, v, _1)); 

    }
    

