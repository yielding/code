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

        cl /nologo /Zm800 /EHsc -I- -I..\..\boost_1_32_0 -c /Fo%TEMP%\metaprogram-chapter9-example8.o example8.cpp

*/
#include <boost/mpl/vector.hpp>
#include <vector>
#include <iterator>
#include <list>

#include <boost/mpl/placeholders.hpp>
#include <boost/mpl/assert.hpp>
#include <boost/static_assert.hpp>
#include <boost/detail/iterator.hpp> // for iterator_traits
namespace mpl = boost::mpl;
using namespace mpl::placeholders;

#include <iostream>
#include <typeinfo>
typedef mpl::vector<int&, long&, char*&> s;


namespace yer {

namespace std
{
  struct input_iterator_tag { };

  struct forward_iterator_tag
    : input_iterator_tag { };

  struct bidirectional_iterator_tag
    : forward_iterator_tag { };

  struct random_access_iterator_tag
    : bidirectional_iterator_tag { };
}

}




    typedef std::vector<int>::iterator I;
    typedef

    boost::detail::iterator_traits<I>::iterator_category

iteratator_category;




    #include <boost/iterator/filter_iterator.hpp>
    namespace my {
    namespace std { 
       using ::boost::detail::iterator_traits; 
       using ::std::input_iterator_tag; 
       using ::std::bidirectional_iterator_tag; 
       using ::std::random_access_iterator_tag; 

  template <class InputIterator, class Distance>
  void __advance_impl(
      InputIterator& i
    , Distance n
    , input_iterator_tag)
  {
      while (n--) ++i;
  }

  template <class BidirectionalIterator, class Distance>
  void __advance_impl(
      BidirectionalIterator& i
    , Distance n
    , bidirectional_iterator_tag)
  {
      if (n >= 0)
        while (n--) ++i;
      else
        while (n++) --i;
  }

  template <class RandomAccessIterator, class Distance>
  void __advance_impl(
      RandomAccessIterator& i
    , Distance n
    , random_access_iterator_tag)
  {
      i += n;
  }

  template <class InputIterator, class Distance>
  void advance(InputIterator& i, Distance n)
  {
      typedef typename
          // The using declaration just doesn't work for vc6
          ::boost::detail::iterator_traits<InputIterator>::iterator_category
      category;

      __advance_impl(i, n, category());
  }
}

    void f(
         ::std::vector<int>::iterator i
       , ::std::list<int>::iterator j
       , boost::filter_iterator<
             bool(*)(int)
           , ::std::vector<int>::iterator
         > k)
    {
        std::advance(i, 4);
        std::advance(i, -4);
        std::advance(j, 4);
        std::advance(j, -4);
        std::advance(k, 4);
    };
    }
    

