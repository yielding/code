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

        cl /nologo /Zm800 /EHsc -I- -I..\..\boost_1_32_0 -c /Fo%TEMP%\metaprogram-chapter5-example104.o example104.cpp

*/
#include <boost/mpl/void.hpp>
#include <boost/mpl/vector.hpp>

#include <boost/mpl/placeholders.hpp>
#include <boost/mpl/assert.hpp>
#include <boost/static_assert.hpp>
namespace mpl = boost::mpl;
using namespace mpl::placeholders;



typedef int T0;
typedef int T1;
typedef int T2;
typedef int T3;
typedef int T4;

namespace boost { namespace python {

  template <class T0=mpl::void_, class T1=mpl::void_, class T2=mpl::void_, class T3=mpl::void_,  class T4=mpl::void_>
  struct bases : mpl::vector<T0,T1,T2,T3,T4> {};

}}
