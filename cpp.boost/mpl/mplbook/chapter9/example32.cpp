/*

    Copyright David Abrahams 2003-2004
    Copyright Aleksey Gurtovoy 2003-2004

    Distributed under the Boost Software License, Version 1.0. 
    (See accompanying file LICENSE_1_0.txt or copy at 
    http://www.boost.org/LICENSE_1_0.txt)
            
    This file was automatically extracted from the source of 
    "C++ Template Metaprogramming", by David Abrahams and 
    Aleksey Gurtovoy.

    It FAILS to compile, as expected, with Microsoft Visual C++ 6.0 SP6
    using the following command: 

        cl /nologo /Zm800 /EHsc -I- -I..\..\boost_1_32_0 -c /Fo%TEMP%\metaprogram-chapter9-example32.o example32.cpp

*/

#include <boost/mpl/placeholders.hpp>
#include <boost/mpl/assert.hpp>
#include <boost/static_assert.hpp>
namespace mpl = boost::mpl;
using namespace mpl::placeholders;


namespace crtp
{
  template <class T>
  struct signed_number
  {
      friend T abs(T x)
      {
          return x < 0 ? -x : x;
      }
  };
}


class Float : crtp::signed_number<Float>
{
 public:
    Float(float x)
      : value(x)
    {}

    Float operator-() const
    {
        return Float(-value);
    }

    bool operator<(float x) const
    {
        return value < x;
    }

    float value;
};

Float const minus_pi = -3.14159265;
Float const pi = abs(minus_pi);


template <class Vehicle>
void drive(Vehicle const& v)
{  }


template <class Screw>
void drive(Screw const& s)
{  }
