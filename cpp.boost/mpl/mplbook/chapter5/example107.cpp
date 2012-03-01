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

        cl /nologo /Zm800 /EHsc -I- -I..\..\boost_1_32_0 -c /Fo%TEMP%\metaprogram-chapter5-example107.o example107.cpp

*/
#include <boost/mpl/next.hpp>
#include <boost/mpl/prior.hpp>

#include <boost/mpl/placeholders.hpp>
#include <boost/mpl/assert.hpp>
#include <boost/mpl/apply_wrap.hpp>  // For mpl::apply_wrap1
#include <boost/static_assert.hpp>
namespace mpl = boost::mpl;
using namespace mpl::placeholders;


struct none {}; // tag type to denote no element
struct tiny_tag {};

template <class T0 = none, class T1 = none, class T2 = none>
struct tiny
{
    typedef tiny type;
    // No partial class template specialization, so we need to use the
    // tag dispatch method.
    typedef tiny_tag tag;

    typedef T0 t0;
    typedef T1 t1;
    typedef T2 t2;

    // ...
};


#include <boost/mpl/iterator_tags.hpp>

// forward declaration
template <class Tiny, int N>
struct tiny_at;


template <class Tiny, class Pos>
struct tiny_iterator
{
    typedef mpl::random_access_iterator_tag category;

    // No partial class template specialization, so we need to use the
    // nested member  method.
    typedef tiny_iterator<
        Tiny
      , typename mpl::next<Pos>::type
    > next;
    
    typedef tiny_iterator<
        Tiny
      , typename mpl::prior<Pos>::type
    > prior;
};


template <int N>
struct tiny_at_impl;

// specialized accessors for each index
struct tiny_at_impl<0>
{
    template <class Tiny>
    struct apply { typedef typename Tiny::t0 type; };
};

struct tiny_at_impl<1>
{
    template <class Tiny>
    struct apply { typedef typename Tiny::t0 type; };
};

struct tiny_at_impl<2>
{
    template <class Tiny>
    struct apply { typedef typename Tiny::t2 type; };
};



template <class Tiny, int N>
struct tiny_at
  : mpl::apply_wrap1<tiny_at_impl<N>, Tiny>
{};


#include <boost/mpl/at.hpp>

namespace boost { namespace mpl {

   template <>
   struct at_impl<tiny_tag>
   {
       template <class Tiny, class N>
       struct apply : tiny_at<Tiny, N::value>
       {};
   };

}}

template<class Sequence, class N>
struct at
  : mpl::apply_wrap2<
        mpl::at_impl<typename Sequence::tag>
      , Sequence, N
    >
{
};
