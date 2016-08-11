/*

    Copyright David Abrahams 2003-2004
    Copyright Aleksey Gurtovoy 2003-2004

    Distributed under the Boost Software License, Version 1.0. 
    (See accompanying file LICENSE_1_0.txt or copy at 
    http://www.boost.org/LICENSE_1_0.txt)
            
    This file was automatically extracted from the source of 
    "C++ Template Metaprogramming", by David Abrahams and 
    Aleksey Gurtovoy.

    It FAILS to compile, as expected, with Microsoft Visual C++ 6.0 SP6
    using the following command: 

        cl /nologo /Zm800 /EHsc -I- -I..\..\boost_1_32_0 -c /Fo%TEMP%\metaprogram-chapter8-example4.o example4.cpp

*/
#include <boost/mpl/vector.hpp>

#include <boost/mpl/placeholders.hpp>
#include <boost/mpl/assert.hpp>
#include <boost/static_assert.hpp>
namespace mpl = boost::mpl;
using namespace mpl::placeholders;


# include <boost/mpl/transform.hpp>
# include <boost/mpl/vector/vector10.hpp>

namespace mpl = boost::mpl;
using namespace mpl::placeholders;
template <class T>
struct returning_ptr
{
    typedef T* type();
};

typedef mpl::transform< 
      mpl::vector5<int&,char,long[5],bool,double>
    , returning_ptr<_1>
 >::type functions;
