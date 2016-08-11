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

        cl /nologo /Zm800 /EHsc -I- -I..\..\boost_1_32_0 -c /Fo%TEMP%\metaprogram-chapter4-example11.o example11.cpp

*/
#include <boost/mpl/eval_if.hpp>
#include <boost/type_traits/is_scalar.hpp>
#include <boost/type_traits/is_reference.hpp>
#include <boost/mpl/identity.hpp>

#include <boost/mpl/placeholders.hpp>
#include <boost/mpl/assert.hpp>
#include <boost/static_assert.hpp>
namespace mpl = boost::mpl;
using namespace mpl::placeholders;



template <class T> struct param_type;

template <class T>
class holder
{
 public:
    holder(typename param_type<T>::type x);
    // ...
 private:
    T x;
};


template <class T>
struct add_reference { typedef T& type; };


#include <boost/mpl/or.hpp>

template <class T>
struct param_type
  : mpl::eval_if<
        mpl::or_<boost::is_scalar<T>, boost::is_reference<T> >
      , mpl::identity<T>
      , add_reference<T const>
    > 
{};

       int a[4];
       holder<int(&)[4]> x(a);
       holder<int> y(a[0]);
       holder<int[4]> z(a);
    

