// Copyright David Abrahams 2004. Distributed under the Boost
// Software License, Version 1.0. (See accompanying
// file LICENSE_1_0.txt or copy at http://www.boost.org/LICENSE_1_0.txt)

#include "boost/mpl/long.hpp"
#include "boost/mpl/alias.hpp"

// VC6 doesn't like integral constant expressions in the base class
// list, so we do the base calculation in the body of a separate class
// template, binary_base.

// binary_base needs this forward declaration
template< long n > struct binary;

template< long n > struct binary_base
{
    // It also has problems with "<<" anywhere within a template
    // argument list so we'll compute an enum and use that instead.
    enum { value = (binary<(n / 10)>::value << 1) + n % 10 };
    typedef mpl::long_<value> type;
};

template< long n > struct binary
   : binary_base<n>::type
{};

template<> struct binary<0>
    : mpl::long_<0>
{};

