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

        cl /nologo /Zm800 /EHsc -I- -I..\..\boost_1_32_0 -c /Fo%TEMP%\metaprogram-chapter5-example108.o example108.cpp

*/
#include <boost/mpl/next.hpp>
#include <boost/mpl/prior.hpp>
#include <boost/mpl/plus.hpp>
#include <boost/mpl/minus.hpp>
#include <boost/mpl/at.hpp>
#include <boost/mpl/begin.hpp>
#include <boost/mpl/end.hpp>

#include <boost/mpl/placeholders.hpp>
#include <boost/mpl/assert.hpp>
#include <boost/mpl/apply_wrap.hpp>  // For mpl::apply_wrap1
#include <boost/static_assert.hpp>
#include <boost/mpl/aux_/nttp_decl.hpp>

namespace mpl = boost::mpl;
using namespace mpl::placeholders;



struct none;
template <class Tiny, BOOST_MPL_AUX_NTTP_DECL(int, N)> struct tiny_at;

struct tiny_tag {};

template <class T0 = none, class T1 = none, class T2 = none>
struct tiny
{
    typedef tiny_tag tag;
    typedef tiny type;
    typedef T0 t0;
    typedef T1 t1;
    typedef T2 t2;
};

namespace boost { namespace mpl {
   template <>
   struct at_impl<tiny_tag>
   {
       template <class Tiny, class N>
       struct apply : tiny_at<Tiny, N::value>
       {};
   };
}}

#include <boost/mpl/iterator_tags.hpp>

// forward declaration
template <class Tiny, int N>
struct tiny_at;


template <class Tiny, class Pos>
struct tiny_iterator
{
    typedef Tiny tiny;
    typedef Pos pos;
    typedef tiny_tag tag;
    
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

    // For deref.
    typedef typename mpl::at<Tiny,Pos>::type type;
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


template<class Sequence, class N>
struct at
  : mpl::apply_wrap2<
        mpl::at_impl<typename Sequence::tag>
      , Sequence, N
    >
{
};



#include <boost/mpl/deref.hpp>

  #include <boost/mpl/advance.hpp>
  #include <boost/mpl/distance.hpp>

namespace boost { namespace mpl {

   // random access iterator requirements
   template <>
   struct advance_impl<tiny_tag>
   {
       template <class TinyIterator, class N>
       struct apply
       {
           typedef tiny_iterator<
               typename TinyIterator::tiny
             , typename mpl::plus<typename TinyIterator::pos,N>::type
           > type;
       };
    };

   template <>
   struct distance_impl<tiny_tag>
   {
       template <class TinyIterator1, class TinyIterator2>
       struct apply
         : mpl::minus<typename TinyIterator2::pos,typename TinyIterator1::pos>
       {};
   };
 
}}


namespace boost { namespace mpl {

   template <>
   struct begin_impl<tiny_tag>
   {
       template <class Tiny>
       struct apply
       {
            typedef tiny_iterator<Tiny,int_<0> > type;
       };
   };
}}



#include <boost/mpl/eval_if.hpp>
#include <boost/mpl/int.hpp>
#include <boost/type_traits/is_same.hpp>

namespace boost { namespace mpl {

   template <>
   struct end_impl<tiny_tag>
   { 
       template <class Tiny>
       struct apply
         : eval_if<
               is_same<none,typename Tiny::t0>
             , int_<0>
             , eval_if<
                   is_same<none,typename Tiny::t1>
                 , int_<1>
                 , eval_if<
                       is_same<none,typename Tiny::t2>
                     , int_<2>
                     , int_<3>
                   >
               >
           >
       {};
   };
}}
