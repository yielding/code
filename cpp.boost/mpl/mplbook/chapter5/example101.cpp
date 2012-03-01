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

        cl /nologo /Zm800 /EHsc -I- -I..\..\boost_1_32_0 -c /Fo%TEMP%\metaprogram-chapter5-example101.o example101.cpp

*/
#include <boost/mpl/map.hpp>
#include <boost/mpl/pair.hpp>

#include <boost/mpl/placeholders.hpp>
#include <boost/mpl/assert.hpp>
#include <boost/static_assert.hpp>
namespace mpl = boost::mpl;
using namespace mpl::placeholders;


template <class T1, class T2>
struct pair
{
    typedef pair type;
    typedef T1 first;
    typedef T2 second;
};


typedef mpl::map<
    mpl::pair<bool, unsigned char>
  , mpl::pair<unsigned char, unsigned short>
  , mpl::pair<unsigned short, unsigned int>
  , mpl::pair<unsigned int, unsigned long>
  , mpl::pair<signed char, signed short>
  , mpl::pair<signed short, signed int>
  , mpl::pair<signed int, signed long>
>::type to_larger;
