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

        cl /nologo /Zm800 /EHsc -I- -I..\..\boost_1_32_0 -c /Fo%TEMP%\metaprogram-chapter3-example35.o example35.cpp

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


#include <boost/mpl/and.hpp>
#include <boost/mpl/not.hpp>
#include <boost/mpl/find_if.hpp>
#include <boost/type_traits/is_convertible.hpp>
#include <boost/type_traits/is_float.hpp>
typedef mpl::vector<char, double, short, long> some_sequence;

// find the position of a type x in some_sequence such that:
//         x is convertible to 'int'
//      && x is not 'char'
//      && x is not a floating type
typedef mpl::find_if<
      some_sequence
    , mpl::and_<
          boost::is_convertible<_1,int>
        , mpl::not_<boost::is_same<_1,char> >
        , mpl::not_<boost::is_float<_1> >
      >
    >::type iter;
