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

        cl /nologo /Zm800 /EHsc -I- -I..\..\boost_1_32_0 -c /Fo%TEMP%\metaprogram-chapter4-example16.o example16.cpp

*/
#include <boost/mpl/integral_c.hpp>

#include <boost/mpl/placeholders.hpp>
#include <boost/mpl/assert.hpp>
#include <boost/static_assert.hpp>
namespace mpl = boost::mpl;
using namespace mpl::placeholders;



namespace cw_bug_workaround {

template<class T, T N>
struct integral_c
{
    // MSVC 6 has trouble with simple integral constant expressions
    // as direct template arguments, so we define next_ and prior_
    // here and use them below.
    enum { value = N, next_ = N+1, prior_ = N-1 };
    typedef integral_c<T,N> type;

    typedef T value_type;

    typedef mpl::integral_c<T,next_> next;
    typedef mpl::integral_c<T,prior_> prior;
    operator T() const { return N; }
};

}

