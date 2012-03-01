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

        cl /nologo /Zm800 /EHsc -I- -I..\..\boost_1_32_0 -c /Fo%TEMP%\metaprogram-chapter5-example111.o example111.cpp

*/
#include <boost/mpl/int.hpp>
#include <boost/mpl/size.hpp>
#include <boost/mpl/at.hpp>

#include <boost/mpl/placeholders.hpp>
#include <boost/mpl/assert.hpp>
#include <boost/static_assert.hpp>
namespace mpl = boost::mpl;
using namespace mpl::placeholders;



template <class T> struct dimensions;

typedef dimensions<char [10][5][2]> seq;
    namespace boost { namespace mpl {
    template <> struct size<seq> : mpl::int_<3> {};
    template <> struct at_c<seq,0> : mpl::int_<2> {};
    template <> struct at_c<seq,1> : mpl::int_<5> {};
    template <> struct at_c<seq,2> : mpl::int_<10> {};
    }}

BOOST_STATIC_ASSERT( mpl::size<seq>::value == 3 );
BOOST_STATIC_ASSERT(( mpl::at_c<seq,0>::type::value == 2 ));
BOOST_STATIC_ASSERT(( mpl::at_c<seq,1>::type::value == 5 ));
BOOST_STATIC_ASSERT(( mpl::at_c<seq,2>::type::value == 10 ));
