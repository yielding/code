// Copyright Tor Brede Vekterli 2008-2009 (vekterli@arcticinteractive.com)
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

// Replacements for boost::lambda::new_ptr, since it has certain quirks that
// make it ill-fitted for our purposes (fixed maximum arity, no result_type
// typedef)
#ifndef BOOST_FACTORY_CONSTRUCT_HPP
#define BOOST_FACTORY_CONSTRUCT_HPP

#include <boost/preprocessor/repetition.hpp>
#include <boost/preprocessor/cat.hpp>
#include <boost/preprocessor/iteration/local.hpp>

#ifndef BOOST_FACTORY_CONSTRUCT_MAX_ARITY
#  define BOOST_FACTORY_CONSTRUCT_MAX_ARITY 10
#endif

namespace boost {  namespace factory {

template <class T>
struct new_ptr
{
  typedef T* result_type;

  T* operator()() const
  {
    return new T;
  }

#define BOOST_FACTORY_CONSTRUCT_operator_param(z, n, unused) \
  BOOST_PP_CAT(A, n)& BOOST_PP_CAT(arg, n)

#define BOOST_FACTORY_CONSTRUCT_operator(z, n, unused) \
  template<BOOST_PP_ENUM_PARAMS(n, typename A)> \
  T* operator()(BOOST_PP_ENUM(n, BOOST_FACTORY_CONSTRUCT_operator_param, ~)) const \
  { \
    return new T(BOOST_PP_ENUM_PARAMS(n, arg)); \
  }

#define BOOST_PP_LOCAL_MACRO(n) BOOST_FACTORY_CONSTRUCT_operator(~, n, ~)
#define BOOST_PP_LOCAL_LIMITS (1, BOOST_FACTORY_CONSTRUCT_MAX_ARITY)
#include BOOST_PP_LOCAL_ITERATE()

#undef BOOST_FACTORY_CONSTRUCT_operator_param
#undef BOOST_FACTORY_CONSTRUCT_operator

};

// Simple functor class analogous to new_ptr, but with the ability to
// wrap return types. Useful as a generic solution for auto_ptr, shared_ptr etc
template <class T, typename Wrapper>
struct wrapped_new_ptr
{
  typedef Wrapper result_type;

  Wrapper operator()() const
  {
    return Wrapper(new T);
  }

#define BOOST_FACTORY_CONSTRUCT_operator_param(z, n, unused) \
  BOOST_PP_CAT(A, n)& BOOST_PP_CAT(arg, n)

#define BOOST_FACTORY_CONSTRUCT_operator(z, n, unused) \
  template<BOOST_PP_ENUM_PARAMS(n, typename A)> \
  Wrapper operator()(BOOST_PP_ENUM(n, BOOST_FACTORY_CONSTRUCT_operator_param, ~)) const \
  { \
    return Wrapper(new T(BOOST_PP_ENUM_PARAMS(n, arg))); \
  }

#define BOOST_PP_LOCAL_MACRO(n) BOOST_FACTORY_CONSTRUCT_operator(~, n, ~)
#define BOOST_PP_LOCAL_LIMITS (1, BOOST_FACTORY_CONSTRUCT_MAX_ARITY)
#include BOOST_PP_LOCAL_ITERATE()

#undef BOOST_FACTORY_CONSTRUCT_operator_param
#undef BOOST_FACTORY_CONSTRUCT_operator

};

} // namespace factory
} // namespace boost

#endif // BOOST_FACTORY_CONSTRUCT_HPP
