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

        cl /nologo /Zm800 /EHsc -I- -I..\..\boost_1_32_0 -c /Fo%TEMP%\metaprogram-chapter3-example19.o example19.cpp

*/

    #include <boost/mpl/int.hpp>
    #include <boost/mpl/vector.hpp>
    #include <boost/mpl/apply_wrap.hpp>


namespace boost{namespace mpl {}}
namespace mpl = boost::mpl;

#include <boost/static_assert.hpp>
#include <boost/mpl/transform.hpp>
#include <boost/mpl/minus.hpp>

#include<boost/mpl/placeholders.hpp>
using namespace mpl::placeholders;

#include <boost/type_traits/add_pointer.hpp>
    #include <boost/static_assert.hpp>
    #include <boost/type_traits/is_same.hpp>

template <class F, class X>
struct twice
{
    typedef typename mpl::apply_wrap1<F,X>::type once;    // f(x)
    typedef typename mpl::apply_wrap1<F,once>::type type; // f(f(x))
};

#include <boost/mpl/assert.hpp>
struct add_pointer_f
{ 
    template <class T> struct apply : boost::add_pointer<T>
    {};
};
BOOST_MPL_ASSERT((boost::is_same<twice<add_pointer_f,int>::type,int**>));

