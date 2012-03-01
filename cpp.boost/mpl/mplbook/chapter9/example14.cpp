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

        cl /nologo /Zm800 /EHsc -I- -I..\..\boost_1_32_0 -c /Fo%TEMP%\metaprogram-chapter9-example14.o example14.cpp

*/
#include <boost/mpl/vector.hpp>
#include <boost/type_traits/is_empty.hpp>

#include <boost/mpl/placeholders.hpp>
#include <boost/mpl/assert.hpp>
#include <boost/static_assert.hpp>
namespace mpl = boost::mpl;
using namespace mpl::placeholders;

#include <iostream>
#include <typeinfo>
typedef mpl::vector<int&, long&, char*&> s;
#include <functional>

// base class template to be defined later

template <bool F_empty, bool G_empty>
struct storage_base
{
    template <class F, class G> class impl;
};

template <class F, bool F_empty, class G, bool G_empty>
struct storage
  : storage_base<F_empty, G_empty>::template impl<F, G>
{
    typedef storage_base<F_empty, G_empty>::template impl<F, G>
    base;

    storage(F const& f, G const& g)
      : base(f,g)
    {};
};

template <class R, class F, class G>
class compose_fg
  : storage<
        F, boost::is_empty<F>::value
      , G,boost::is_empty<G>::value
    >
{
   typedef storage<
       F,boost::is_empty<F>::value
     , G,boost::is_empty<G>::value
   > base;

 public:
    compose_fg(F const& f, G const& g)
      : base(f, g)
    {}

    template <class T>
    R operator()(T const& x) const
    {
        F const& f = this->get_f();
        G const& g = this->get_g();
        return f(g(x));
    }
};


template <>
struct storage_base<false,false> // neither F nor G is empty
{
    template <class F, class G>
    class impl 
    {
     protected:
        impl(F const& f, G const& g)
          : f(f), g(g)
        {}
        F const& get_f() const { return f; }
        G const& get_g() const { return g; }
     private:
        F f;
        G g;
    };
};

template <>
struct storage_base<false,true> // G is empty
{
    template <class F, class G>
    class impl 
      : private G
    {
     protected:
        impl(F const& f, G const& g)
          : G(g), f(f)
        {}
        F const& get_f() const { return f; }
        G const& get_g() const { return *this; }
     private:
        F f;
    };
};

struct storage_base<true,false> // F is empty
{
    template <class F, class G>
    class impl
      : private F
    {
     protected:
        impl(F const& f, G const& g)
          : F(f), g(g)
        {}
        F const& get_f() const { return *this; }
        G const& get_g() const { return g; }
     private:
        G g;
    };
};

template <>
struct storage_base<true,true> // F and G are both empty
{
    template <class F, class G>
    class impl
      : private F, private G
    {
     protected:
        impl(F const& f, G const& g)
          : F(f), G(g)
        {}
        F const& get_f() const { return *this; }
        G const& get_g() const { return *this; }
    };
};

 template <class T> T make();
 int x
   = make<compose_fg<int, std::negate<int>, std::logical_not<int> > >()(1)
   + make<compose_fg<int, int (*)(int), std::negate<int> > >()(2)
   + make<compose_fg<int, std::negate<int>, int (*)(int)> >()(3)
   + make<compose_fg<int, int (*)(int), int (*)(int)> >()(4);

