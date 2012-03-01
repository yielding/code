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

        cl /nologo /Zm800 /EHsc -I- -I..\..\boost_1_32_0 -c /Fo%TEMP%\metaprogram-chapter10-example12.o example12.cpp

*/


  #include <boost/array.hpp>
  struct Array
    : boost::array<float,100>
  {};

// operation tags
struct plus; struct minus; 

// expression tree node
template <class L, class OpTag, class R> 
struct Expression;

// addition operator
template <class L, class R>
Expression<L,plus,R> operator+(L const& l, R const& r)
{
    return Expression<L,plus,R>(l, r);
}



typedef 

Expression<Expression<Array,plus,Array>,plus,Array>

plus_result;



// operation tags implement elementwise arithmetic
struct plus
{
  static float apply(float a, float b)
    { return a + b; }
};

struct minus
{
  static float apply(float a, float b)
    { return a - b; }
};


// expression tree node
template <class L, class OpTag, class R>
struct Expression
{
    Expression(L const& l, R const& r)
      : l(l), r(r) {}

    float operator[](unsigned index) const
    {
      return OpTag::apply(l[index], r[index]);
    }

    L const& l;
    R const& r;
};



      bool index_test(Array const& a, Array const& b, Array const& c)
      {
          return

(a + b)[1] 
  == plus::apply(a[1], b[1]) 
  == a[1] + b[1]

;
      }
    

