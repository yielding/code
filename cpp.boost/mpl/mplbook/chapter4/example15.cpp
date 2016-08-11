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

        cl /nologo /Zm800 /EHsc -I- -I..\..\boost_1_32_0 -c /Fo%TEMP%\metaprogram-chapter4-example15.o example15.cpp

*/
#include <boost/mpl/int.hpp>

#include <boost/mpl/placeholders.hpp>
#include <boost/mpl/assert.hpp>
#include <boost/mpl/aux_/nttp_decl.hpp> // For BOOST_MPL_AUX_NTTP_DECL
#include <boost/static_assert.hpp>

namespace mpl = boost::mpl;
using namespace mpl::placeholders;



namespace cw_bug_workaround {

// MSVC6 sometimes chokes on non-type template parameters
template< BOOST_MPL_AUX_NTTP_DECL(int, N) >
struct int_
{
    // MSVC 6 has trouble with simple integral constant expressions
    // as direct template arguments, so we define next_ and prior_
    // here and use them below.
    enum { value = N, next_ = N+1, prior_ = N-1 };
    typedef int_<N> type;

    typedef int value_type;

    typedef mpl::int_<next_> next;
    typedef mpl::int_<prior_> prior;

    operator int() const { return N; }
};

}

