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

        cl /nologo /Zm800 /EHsc -I- -I..\..\boost_1_32_0 -c /Fo%TEMP%\metaprogram-chapter4-example5.o example5.cpp

*/
#include <boost/type_traits/is_scalar.hpp>

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


#include <boost/mpl/if.hpp>
#include <boost/mpl/identity.hpp>
#include <boost/type_traits/add_reference.hpp>

template <class T>
struct param_type
  : mpl::if_<           // forwarding to selected transformation
        typename boost::is_scalar<T>::type
      , mpl::identity<T>
      , boost::add_reference<T const>
    >::type
{};

       int a[4];
       holder<int(&)[4]> x(a);
       holder<int> y(a[0]);
       holder<int[4]> z(a);
    

