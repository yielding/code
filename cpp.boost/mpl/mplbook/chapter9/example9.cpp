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

        cl /nologo /Zm800 /EHsc -I- -I..\..\boost_1_32_0 -c /Fo%TEMP%\metaprogram-chapter9-example9.o example9.cpp

*/
#include <boost/mpl/vector.hpp>
#include <boost/mpl/bool.hpp>
#include <boost/type_traits/is_polymorphic.hpp>
#include <boost/type_traits/remove_pointer.hpp>

#include <boost/mpl/placeholders.hpp>
#include <boost/mpl/assert.hpp>
#include <boost/static_assert.hpp>
namespace mpl = boost::mpl;
using namespace mpl::placeholders;

#include <iostream>
#include <typeinfo>
typedef mpl::vector<int&, long&, char*&> s;

// implementation for polymorphic types
template <class T, class U>
T desperate_cast_impl2(U& x, mpl::true_)
{
    return dynamic_cast<T>(x); // legal iff U is polymorphic
}

// implementation for non-polymorphic types
template <class T, class U>
T desperate_cast_impl2(U& x, mpl::false_)
{
    return static_cast<T>(x);
}

// dispatcher
template <class T, class U>
T desperate_cast_impl(U& x)
{
    return desperate_cast_impl2<T>(
        x
      , boost::is_polymorphic<
#if !defined(BOOST_MSVC) || BOOST_MSVC > 1200
            typename
#endif 
            boost::remove_pointer<U>::type
        >()
    );
}

// public interface
template <class T, class U>
T desperate_cast(U const& x) { return desperate_cast_impl<T>(x); }

template <class T, class U>
T desperate_cast(U& x) { return desperate_cast_impl<T>(x); }

    class A {};
    class B { public: virtual ~B(); };
    class C : public A {};
    class D : public A, public B {};
    void f(A* a, B* b, C* c, D* d)
    {
        desperate_cast<C*>(a);
        desperate_cast<D*>(a);
        desperate_cast<D*>(b);
        desperate_cast<A*>(c);
        desperate_cast<A*>(d);
        desperate_cast<B*>(d);
    }

