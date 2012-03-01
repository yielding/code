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

        cl /nologo /Zm800 /EHsc -I- -I..\..\boost_1_32_0 -c /Fo%TEMP%\metaprogram-chapter9-example15.o example15.cpp

*/
#include <boost/mpl/vector.hpp>
#include <boost/type_traits/is_same.hpp>
#include <boost/mpl/fold.hpp>

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
    

// fine-grained struct element; stores a T and inherits More
template <class T, class More>
struct store : More
{
    T value;
    typedef store type;
    BOOST_MPL_AUX_LAMBDA_SUPPORT(2, store, (T,More))
};

typedef mpl::vector<short[2], long, char*, int> member_types;

struct empty {};

mpl::fold<
    member_types, empty, store<_2,_1>
>::type generated;


    namespace test_type_of0
    { //}
      int x = test_type_of<

store<int
  , store<char*
      , store<long
          , store<short[2], empty> > > >

>(
         generated
      ); //{
    }
    

long& x = static_cast<
            store<long, store<short[2], empty> >&
          >(generated).value;



namespace eggs {

template <class T, class U>
store<T,U> const& get(store<T,U> const& e)
{ 
    return e;
}

// MSVC 6.6/7.0 fails argument deduction here.  We don't have a workaround.
char* s = get<char*>(generated).value;

}
