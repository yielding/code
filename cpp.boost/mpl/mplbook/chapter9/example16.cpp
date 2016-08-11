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

        cl /nologo /Zm800 /EHsc -I- -I..\..\boost_1_32_0 -c /Fo%TEMP%\metaprogram-chapter9-example16.o example16.cpp

*/
#include <boost/mpl/vector.hpp>
#include <boost/type_traits/is_same.hpp>
#include <boost/mpl/fold.hpp>
#include <boost/mpl/empty_base.hpp>

#include <boost/mpl/placeholders.hpp>
#include <boost/mpl/assert.hpp>
#include <boost/static_assert.hpp>
namespace mpl = boost::mpl;
using namespace mpl::placeholders;

#include <iostream>
#include <typeinfo>
typedef mpl::vector<int&, long&, char*&> s;
#include <functional>

    template <class T, class U>
    int test_type_of(U const&)
    {
        BOOST_MPL_ASSERT((boost::is_same<T,U>));
        return 0;
    }
    


namespace byhand {

// fine-grained struct element; wraps a T
template <class T>
struct wrap
{
    T value;
    typedef wrap type;
    BOOST_MPL_AUX_LAMBDA_SUPPORT(1, wrap, (T))
};

// one more level of indirection
template <class U, class V>
struct inherit_ : U, V
{
    typedef inherit_ type;
    BOOST_MPL_AUX_LAMBDA_SUPPORT(2, inherit_, (U,V))
};

typedef mpl::vector<short[2], long, char*, int> member_types;

struct empty {};

mpl::fold<
    member_types, empty, inherit_<wrap<_2>,_1>
>::type generated;


    namespace test_type_of1
    { //}
      int x = test_type_of<

inherit_<wrap<int>
  , inherit_<wrap<char*>
      , inherit_<wrap<long>
          , inherit_<wrap<short[2]>
              , empty
            >
        >
    >
>

>(
         generated
      ); //{
    }
    

long& x = static_cast<wrap<long>&>(generated).value;

}




namespace my { 
    using mpl::empty_base;
    using mpl::fold;

template <class Types, class Node, class Root = empty_base>
struct inherit_linearly
  : fold<Types,Root,Node>
{
};

}



#include <boost/mpl/inherit.hpp>
#include <boost/mpl/inherit_linearly.hpp>
#include <boost/mpl/vector.hpp>

// fine-grained struct element
template <class T>
struct wrap
{
    T value;
    typedef wrap type;
    BOOST_MPL_AUX_LAMBDA_SUPPORT(1, wrap, (T))
};

typedef mpl::vector<short[2], long, char*, int> member_types;

mpl::inherit_linearly<
    member_types, mpl::inherit<wrap<_2>,_1>
>::type generated;

using mpl::inherit2;



    namespace test_type_of2
    { //}
      int x = test_type_of<
inherit2<wrap<int>, inherit2<wrap<char*>,inherit2<wrap<long>,wrap<short[2]> > > >
>(
         generated
      ); //{
    }
    
