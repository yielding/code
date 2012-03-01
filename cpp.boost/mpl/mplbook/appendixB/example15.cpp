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

        cl /nologo /Zm800 /EHsc -I- -I..\..\boost_1_32_0 -c /Fo%TEMP%\metaprogram-appendixB-example15.o example15.cpp

*/
#include <iterator>

#include <boost/mpl/placeholders.hpp>
#include <boost/mpl/assert.hpp>
#include <boost/static_assert.hpp>
#include <boost/mpl/aux_/nttp_decl.hpp>
namespace mpl = boost::mpl;
using namespace mpl::placeholders;



namespace {

template <class Iterator, BOOST_MPL_AUX_NTTP_DECL(long, N)> struct index
{
#if !defined(BOOST_MSVC) || BOOST_MSVC > 1300
    typedef char(&type)[N];
#else
    typedef char type[N];
#endif
};

template <class Iterator>
struct category_index
{
    int category(std::input_iterator_tag)
    { return sizeof(typename index<Iterator,1>::type); }

    int category(std::forward_iterator_tag)
    { return sizeof(typename index<Iterator,2>::type); }
};

}

