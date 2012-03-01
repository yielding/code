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

        cl /nologo /Zm800 /EHsc -I- -I..\..\boost_1_32_0 -c /Fo%TEMP%\metaprogram-chapter5-example110.o example110.cpp

*/
#include <boost/mpl/identity.hpp>
#include <boost/mpl/equal.hpp>
#include <boost/mpl/vector_c.hpp>

#include <boost/mpl/placeholders.hpp>
#include <boost/mpl/assert.hpp>
#include <boost/static_assert.hpp>
namespace mpl = boost::mpl;
using namespace mpl::placeholders;



    template <class T>
    struct double_first_half : mpl::identity<T> {};
    enum { tst = 

mpl::equal<
    double_first_half< mpl::vector_c<int,1,2,3,4> >::type
  , mpl::vector_c<int,2,4,3,4>
>::type::value

}; BOOST_STATIC_ASSERT(!tst);

