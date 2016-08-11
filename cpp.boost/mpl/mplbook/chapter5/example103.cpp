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

        cl /nologo /Zm800 /EHsc -I- -I..\..\boost_1_32_0 -c /Fo%TEMP%\metaprogram-chapter5-example103.o example103.cpp

*/
#include <boost/mpl/vector.hpp>

#include <boost/mpl/placeholders.hpp>
#include <boost/mpl/assert.hpp>
#include <boost/mpl/aux_/nttp_decl.hpp> // for BOOST_MPL_AUX_NTTP_DECL
#include <boost/static_assert.hpp>
namespace mpl = boost::mpl;
using namespace mpl::placeholders;



  #include <boost/mpl/vector/vector10.hpp>
  using boost::mpl::vector0;
using boost::mpl::vector1;
using boost::mpl::vector2;
using boost::mpl::vector3;
using boost::mpl::vector4;
  namespace protect {
  

namespace boost { namespace mpl {

  namespace mpl_ = ::boost::mpl;

  struct void_; // "no argument" marker

  // VC6 sometimes chokes on non-type template parameters; this macro
  // is a workaround.
  template < BOOST_MPL_AUX_NTTP_DECL(int, N) >
  struct vector_base;

  template<>
  struct vector_base<0>
  {
      template <class T0, class T1, class T2, class T3>
      struct apply
      {
          typedef vector0<> type;
      };
  };

  template<>
  struct vector_base<1>
  {
      template <class T0, class T1, class T2, class T3>
      struct apply
      {
          typedef vector1<T0> type;
      };
  };

  template<>
  struct vector_base<2>
  {
      template <class T0, class T1, class T2, class T3>
      struct apply
      {
          typedef vector2<T0,T1> type;
      };
  };

  template<>
  struct vector_base<3>
  {
      template <class T0, class T1, class T2, class T3>
      struct apply
      {
          typedef vector3<T0,T1,T2> type;
      };
  };

  template<>
  struct vector_base<4>
  {
      template <class T0, class T1, class T2, class T3>
      struct apply
      {
          typedef vector4<T0,T1,T2,T3> type;
      };
  };

  template <class T0, class T1, class T2, class T3>
  struct sum_void
  {
      enum { value =
             ::boost::is_same<T0,void_>::value
             + ::boost::is_same<T1,void_>::value
             + ::boost::is_same<T2,void_>::value
             + ::boost::is_same<T3,void_>::value
      };
  };

  template <class T0 = void_, class T1 = void_, /* etc. */class T2 = void_, class T3 = void_>
  struct vector
    // No support for partial specialization, so we'll use the classic workaround.
    : vector_base<sum_void<T0,T1,T2,T3>::value>::apply<T0,T1,T2,T3>::type
  {};

  // specializations

  /* etc. */
}}

}

