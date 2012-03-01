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

        cl /nologo /Zm800 /EHsc -I- -I..\..\boost_1_32_0 -c /Fo%TEMP%\metaprogram-chapter5-example109.o example109.cpp

*/
#include <boost/mpl/next.hpp>
#include <boost/mpl/prior.hpp>
#include <boost/mpl/plus.hpp>
#include <boost/mpl/minus.hpp>
#include <boost/mpl/int.hpp>
#include <boost/mpl/at.hpp>
#include <boost/mpl/begin.hpp>
#include <boost/mpl/end.hpp>
#include <boost/mpl/size.hpp>
#include <boost/mpl/clear.hpp>
#include <boost/mpl/push_front.hpp>
#include <boost/mpl/push_back.hpp>

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
struct tiny_at_impl
{
    template <class Tiny>
    struct apply { typedef mpl::void_ type; };
};

// specialized accessors for each index
struct tiny_at_impl<0>
{
    template <class Tiny>
    struct apply { typedef typename Tiny::t0 type; };
};

struct tiny_at_impl<1>
{
    template <class Tiny>
    struct apply { typedef typename Tiny::t1 type; };
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

template <class T> struct wrap {};

template <class T0, class T1, class T2>
struct tiny_size
{
    typedef char(&three)[4];
    typedef char(&two)[3];
    typedef char(&one)[2];
    typedef char zero;
    
    static three test(...);
    static two test(wrap<none>, ...);
    static one test(wrap<none>, wrap<none>, ...);
    static zero test(wrap<none>, wrap<none>, wrap<none>);

    enum {
        value = sizeof(
            test( wrap<T2>(), wrap<T1>(), wrap<T0>() )
        ) - 1
    };
    
    typedef mpl::int_<value> type;
};

namespace boost { namespace mpl {
   template <>
   struct end_impl<tiny_tag>
   {
       template <class Tiny>
       struct apply
       {
           typedef tiny_iterator<
               Tiny
             , typename tiny_size<
                   typename Tiny::t0
                 , typename Tiny::t1
                 , typename Tiny::t2
               >::type
           >
           type;
       };
   };
}}


namespace boost { namespace mpl {
   template <>
   struct size_impl<tiny_tag>
   {
       template <class Tiny>
       struct apply
         : tiny_size<
               typename Tiny::t0
             , typename Tiny::t1
             , typename Tiny::t2
           >
       {};
   };
}}


namespace boost { namespace mpl {
   template <>
   struct clear_impl<tiny_tag>
   {
       template <class Tiny>
       struct apply : tiny<>
       {};
   };
   template <>
   struct push_front_impl<tiny_tag>
   {
       template <class Tiny, class T>
       struct apply
         : tiny<T, typename Tiny::t0, typename Tiny::t1>
       {};
   };
}}


template <int N>
struct tiny_push_back
{
    template <class Tiny, class T> struct result_;
};

template <>
struct tiny_push_back<0>
{
    template <class Tiny, class T>
    struct result_ : tiny<T,none,none>
    {};
};

template <>
struct tiny_push_back<1>
{
    template <class Tiny, class T>
    struct result_ : tiny<typename Tiny::t0,T,none>
    {};
};

template <>
struct tiny_push_back<2>
{
    template <class Tiny, class T>
    struct result_ : tiny<typename Tiny::t0,typename Tiny::t1,T>
    {};
};

namespace boost { namespace mpl {
   template <>
   struct push_back_impl<tiny_tag>
   {
       template <class Tiny, class T>
       struct apply
         : tiny_push_back<size<Tiny>::value>::result_<Tiny,T>
       {};
   };
}}

#include <boost/mpl/push_back.hpp>
#include <boost/mpl/pop_back.hpp>
#include <boost/mpl/push_front.hpp>
#include <boost/mpl/pop_front.hpp>
#include <boost/mpl/back.hpp>
#include <boost/mpl/front.hpp>
#include <boost/mpl/size.hpp>
#include <boost/mpl/empty.hpp>

void f()
{
    typedef tiny<> v0;
    typedef tiny<char> v1;
    typedef tiny<char,long> v2;
    typedef tiny<char,char,int> v3;

    BOOST_MPL_ASSERT_RELATION(mpl::size<v0>::value, ==, 0);
    BOOST_MPL_ASSERT_RELATION(mpl::size<v1>::value, ==, 1);
    BOOST_MPL_ASSERT_RELATION(mpl::size<v2>::value, ==, 2);
    BOOST_MPL_ASSERT_RELATION(mpl::size<v3>::value, ==, 3);

    BOOST_MPL_ASSERT(( mpl::empty<v0> ));
    BOOST_MPL_ASSERT_NOT(( mpl::empty<v1> ));
    BOOST_MPL_ASSERT_NOT(( mpl::empty<v2> ));
    BOOST_MPL_ASSERT_NOT(( mpl::empty<v3> ));

    BOOST_MPL_ASSERT((boost::is_same<mpl::front<v1>::type,char>));
    BOOST_MPL_ASSERT((boost::is_same<mpl::back<v1>::type,char>));
    BOOST_MPL_ASSERT((boost::is_same<mpl::front<v2>::type,char>));
    BOOST_MPL_ASSERT((boost::is_same<mpl::back<v2>::type,long>));
    BOOST_MPL_ASSERT((boost::is_same<mpl::front<v3>::type,char>));
    BOOST_MPL_ASSERT((boost::is_same<mpl::back<v3>::type,int>));
}

void g()
{
    typedef tiny<> v0;

    typedef mpl::push_back<v0,int>::type v1;
    BOOST_MPL_ASSERT((boost::is_same<mpl::back<v1>::type,int>));

    typedef mpl::push_front<v1,char>::type v2;
    BOOST_MPL_ASSERT((boost::is_same<mpl::back<v2>::type,int>));
    BOOST_MPL_ASSERT((boost::is_same<mpl::front<v2>::type,char>));

    typedef mpl::push_back<v2,long>::type v3;
    BOOST_MPL_ASSERT((boost::is_same<mpl::back<v3>::type,long>));
}

void h()
{
    typedef tiny<> v0;
    typedef tiny<char> v1;
    typedef tiny<char,long> v2;
    typedef tiny<char,char,char> v3;

    BOOST_MPL_ASSERT_RELATION(mpl::size<v0>::value, ==, 0);
    BOOST_MPL_ASSERT_RELATION(mpl::size<v1>::value, ==, 1);
    BOOST_MPL_ASSERT_RELATION(mpl::size<v2>::value, ==, 2);
    BOOST_MPL_ASSERT_RELATION(mpl::size<v3>::value, ==, 3);
}


