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

        cl /nologo /Zm800 /EHsc -I- -I..\..\boost_1_32_0 -c /Fo%TEMP%\metaprogram-chapter7-example1.o example1.cpp

*/
#include <boost/mpl/sizeof.hpp>
#include <boost/mpl/deref.hpp>
#include <boost/mpl/lower_bound.hpp>
#include <boost/mpl/less.hpp>
#include <boost/mpl/vector.hpp>
#include <boost/mpl/int.hpp>
#include <boost/type_traits/is_same.hpp>
#include <boost/mpl/prior.hpp>
#include <boost/mpl/begin_end.hpp>
#include <boost/mpl/equal_to.hpp>
#include <boost/mpl/apply.hpp>
#include <boost/mpl/transform.hpp>
#include <boost/mpl/transform_view.hpp>

#include <boost/mpl/placeholders.hpp>
#include <boost/mpl/assert.hpp>
#include <boost/static_assert.hpp>
namespace mpl = boost::mpl;
using namespace mpl::placeholders;



namespace n1 {

template<class Seq, class MinSize>
struct padded_size
  : mpl::sizeof_<                       // the size of
        typename mpl::deref<            // the element at
            typename mpl::lower_bound<  // the first position
                Seq
              , MinSize
              , mpl::less<mpl::sizeof_<_1>,_2>  // satisfying...
            >::type
        >::type
    >
{};

}




typedef 

mpl::less<mpl::sizeof_<_1>, _2>

Pred;




  typedef mpl::vector<char, char(&)[2], char(&)[4]> Sequence;
  typedef mpl::int_<3> T;
  using mpl::lower_bound;
  

typedef mpl::lower_bound< Sequence,T,Pred >::type i; 

  BOOST_MPL_ASSERT((
    boost::is_same<
        i
      , mpl::prior<mpl::end<Sequence>::type>::type
    >));

  typedef mpl::sizeof_<mpl::deref<i>::type>::type sz;

  BOOST_MPL_ASSERT((
    mpl::equal_to<
        sz
      , n1::padded_size<Sequence,T>::type
    >));




  typedef mpl::prior<i>::type j;
  enum { tst = 

mpl::apply2<Pred, mpl::deref<j>::type, T >::type::value

};
  BOOST_STATIC_ASSERT(tst);
  



template<class Seq, class MinSize>
struct padded_size
  : mpl::sizeof_<
        typename mpl::deref<
            typename mpl::lower_bound<
                Seq
              , MinSize
              , mpl::less<mpl::sizeof_<_1>, _2>
            >::type
        >::type
    >
{};

  BOOST_MPL_ASSERT((boost::is_same<sz,padded_size<Sequence,T>::type>));




namespace slow {

template<class Seq, class MinSize>
struct padded_size
  : mpl::deref<
        typename mpl::lower_bound<
            typename mpl::transform<
                Seq, mpl::sizeof_<_>
            >::type
          , MinSize
          , mpl::less<_,_>
        >::type
    >
{};

       BOOST_MPL_ASSERT((
         mpl::equal_to<
             padded_size<Sequence,T>::type
           , ::padded_size<Sequence,T>::type
         >));
       }
       




namespace small_slow {

template<class Seq, class MinSize>
struct padded_size
  : mpl::deref<
        typename mpl::lower_bound<
            typename mpl::transform<
                Seq, mpl::sizeof_<_>
            >::type
          , MinSize
        >::type
    >
{};

       BOOST_MPL_ASSERT((
         mpl::equal_to<
             padded_size<Sequence,T>::type
           , ::padded_size<Sequence,T>::type
         >));
       }
       




namespace fast {

template<class Seq, class MinSize>
struct padded_size
  : mpl::deref<
        typename mpl::lower_bound<
            mpl::transform_view<Seq, mpl::sizeof_<_> >
          , MinSize
        >::type
    >
{};

       BOOST_MPL_ASSERT((
         mpl::equal_to<
             padded_size<Sequence,T>::type
           , ::padded_size<Sequence,T>::type
         >));
       }
       

