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

        cl /nologo /Zm800 /EHsc -I- -I..\..\boost_1_32_0 -c /Fo%TEMP%\metaprogram-chapter7-example6.o example6.cpp

*/
#include <boost/mpl/transform.hpp>
#include <boost/mpl/deref.hpp>
#include <boost/mpl/iterator_range.hpp>
#include <boost/mpl/begin_end.hpp>
#include <boost/mpl/vector.hpp>
#include <boost/mpl/range_c.hpp>
#include <boost/mpl/equal.hpp>
#include <boost/mpl/zip_view.hpp>

#include <boost/mpl/placeholders.hpp>
#include <boost/mpl/assert.hpp>
#include <boost/static_assert.hpp>
namespace mpl = boost::mpl;
using namespace mpl::placeholders;


template <class IteratorSeq>
struct zip_iterator;


template <class IteratorSeq>
struct zip_iterator
{
    typedef mpl::forward_iterator_tag category;

    typedef typename mpl::transform<
        IteratorSeq
      , mpl::deref<_1>
    >::type type;

      typedef zip_iterator<
          typename mpl::transform<
              IteratorSeq
            , mpl::next<_1>
          >::type
      > next;
};

template <class Sequences>
struct zip_view
  : mpl::iterator_range<
        zip_iterator<
            typename mpl::transform<
                Sequences, mpl::begin<_1>
            >::type
        >
      , zip_iterator<
            typename mpl::transform<
                Sequences, mpl::end<_1>
            >::type
        >
    >
{};

typedef mpl::vector<
    mpl::range_c<int,0,4>, mpl::range_c<int,7,11> 
> v;
BOOST_MPL_ASSERT((mpl::equal<zip_view<v>,mpl::zip_view<v> >));

