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

        cl /nologo /Zm800 /EHsc -I- -I..\..\boost_1_32_0 -c /Fo%TEMP%\metaprogram-chapter4-example12.o example12.cpp

*/
#include <boost/mpl/vector.hpp>
#include <string>
#include <boost/mpl/transform.hpp>
#include <boost/mpl/if.hpp>
#include <boost/mpl/or.hpp>
#include <boost/type_traits/is_scalar.hpp>
#include <boost/type_traits/is_reference.hpp>
#include <boost/mpl/identity.hpp>
#include <boost/type_traits/add_const.hpp>
#include <boost/type_traits/is_same.hpp>
#include <boost/mpl/front.hpp>
#include <boost/mpl/at.hpp>

#include <boost/mpl/placeholders.hpp>
#include <boost/mpl/assert.hpp>
#include <boost/static_assert.hpp>
namespace mpl = boost::mpl;
using namespace mpl::placeholders;


template <class T>
struct add_reference
{
    typedef T& type;
    BOOST_MPL_AUX_LAMBDA_SUPPORT(1, add_reference, (T))
};


typedef mpl::vector<int, long, std::string> argument_types;

typedef mpl::transform<
    argument_types
  , mpl::if_<
        mpl::or_<boost::is_scalar<_1>, boost::is_reference<_1> >
      , mpl::identity<_1>
      , add_reference<boost::add_const<_1> >
    > 
>::type param_types;

BOOST_MPL_ASSERT((boost::is_same<mpl::front<param_types>::type,int>));
BOOST_MPL_ASSERT((boost::is_same<mpl::at_c<param_types,2>::type,std::string const&>));

