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

        cl /nologo /Zm800 /EHsc -I- -I..\..\boost_1_32_0 -c /Fo%TEMP%\metaprogram-chapter3-example30.o example30.cpp

*/

    #include <boost/mpl/int.hpp>
    #include <boost/mpl/vector.hpp>


namespace boost{namespace mpl {}}
namespace mpl = boost::mpl;

#include <boost/static_assert.hpp>
#include <boost/mpl/transform.hpp>
#include <boost/mpl/minus.hpp>

#include<boost/mpl/placeholders.hpp>
using namespace mpl::placeholders;

#include <boost/type_traits/add_pointer.hpp>
    #include <boost/static_assert.hpp>
    #include <boost/type_traits/is_same.hpp>
#include <boost/mpl/lambda.hpp>
#include <boost/mpl/apply.hpp>
#include <boost/mpl/plus.hpp>
#include <boost/mpl/multiplies.hpp>


namespace shield {

namespace boost { namespace mpl { namespace placeholders {

template <int N> struct arg; // forward declarations
struct void_;

template <>
struct arg<1>
{
    template <
      class A1, class A2 = void_,  class A/* m */ = void_>
    struct apply
    {
        typedef A1 type; // return the first argument
    };
};
typedef arg<1> _1;

template <>
struct arg<2>
{
    template <
      class A1, class A2, class A3 = void_, class A/* m */ = void_
    >
    struct apply
    {
        typedef A2 type; // return the second argument
    };
};
typedef arg<2> _2;

/* more specializations and typedefs */

}}}


namespace boost { namespace mpl { namespace placeholders {

typedef arg<-1> _; // the unnamed placeholder

}}}

}

