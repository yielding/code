// Copyright Tor Brede Vekterli 2008-2009 (vekterli@arcticinteractive.com)
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

// Alexandrescu's factory pattern with value->creator function mappings
#ifndef BOOST_FACTORY_FACTORY_HPP
#define BOOST_FACTORY_FACTORY_HPP

#include <map>
#include <stdexcept>
#include <boost/config.hpp>
#include <boost/function.hpp>
#include <boost/call_traits.hpp>
#include <boost/mpl/if.hpp>
#include <boost/mpl/eval_if.hpp>
#include <boost/mpl/at.hpp>
#include <boost/mpl/int.hpp>
#include <boost/mpl/identity.hpp>
#include <boost/mpl/apply.hpp>
#include <boost/mpl/transform.hpp>
#include <boost/type_traits.hpp>
#include <boost/parameter/preprocessor.hpp>
#include <boost/parameter/parameters.hpp>
#include <boost/parameter/name.hpp>
#include "util.hpp"
#include "detail/meta.hpp"

namespace boost { namespace factory {

BOOST_PARAMETER_TEMPLATE_KEYWORD(func_sig)
BOOST_PARAMETER_TEMPLATE_KEYWORD(id_type)
BOOST_PARAMETER_TEMPLATE_KEYWORD(map_type)
BOOST_PARAMETER_TEMPLATE_KEYWORD(error_policy)

struct exception_on_missing_id
  : detail::error_policy_meta_base
{
  template <typename R, typename Id>
  R on_missing(Id) const
  {
    throw std::invalid_argument("Unknown factory creator function identifier");
  }
};

struct std_map_adapter
  : detail::map_adapter_meta_base
{
  template <typename Key, typename Value>
  struct apply
  {
    typedef std::map<Key, Value> type;
  };
};

#ifdef BOOST_FACTORY_TEMPLATE_AUTO_DEDUCE_PARAMETERS
#include <boost/mpl/logical.hpp>
// Parameters with auto-deduction. Requires that all map adapters derive from
// detail::factory::map_adapter_meta_base and that all error policies derive
// from detail::error_policy_meta_base
typedef parameter::parameters<
    parameter::optional<
        tag::id_type
      , mpl::not_<
          mpl::or_<
              is_base_and_derived<detail::map_adapter_meta_base, mpl::_>
            , is_base_and_derived<detail::error_policy_meta_base, mpl::_>
          >
        >
    >
  , parameter::optional<
        tag::map_type
      , is_base_and_derived<detail::map_adapter_meta_base, mpl::_>
    >
  , parameter::optional<
        tag::error_policy
      , is_base_and_derived<detail::error_policy_meta_base, mpl::_>
    >
> factory_signature_t;
#else
// Parameters without auto-deduction.
typedef parameter::parameters<
    parameter::optional<tag::id_type>
  , parameter::optional<tag::map_type>
  , parameter::optional<tag::error_policy>
> factory_signature_t;
#endif

namespace detail
{

// Signature: either a function signature OR a valid function object type
// Id: a type used for looking up creators in the associative container
// MapAdapter: an MPL metafunction that takes <key, value>
// ErrorPolicy: a class with a nested on_missing<R,T> function
template<
    typename Signature
  , typename Id
  , typename MapAdapter
  , typename ErrorPolicy
>
class factory_impl
{
public:
  // If Signature is a function, it will be stored in a Boost.Function object.
  // Otherwise, it is assumed that the supplied type is a valid function
  // object, and will be used instead
  typedef BOOST_DEDUCED_TYPENAME mpl::if_<
      is_function<Signature>
    , boost::function<Signature>
    , Signature
  >::type functor_type;

  typedef Id id_type;
  typedef ErrorPolicy error_policy_type;
  typedef BOOST_DEDUCED_TYPENAME boost::call_traits<functor_type>::param_type
      functor_param_type;
  typedef BOOST_DEDUCED_TYPENAME boost::call_traits<id_type>::param_type
      id_param_type;
  typedef BOOST_DEDUCED_TYPENAME functor_type::result_type result_type;

private:
  typedef BOOST_DEDUCED_TYPENAME mpl::apply<MapAdapter, id_type, functor_type>::type
      map_type;
  /*typedef BOOST_DEDUCED_TYPENAME mpl::apply<error_policy_type, functor_type, Id>::type
      error_policy_impl_type;*/

  map_type mappings_;
  // Have the error policy be a member, so that it may store default functions
  // and whatnot
  error_policy_type error_policy_;

public:
  factory_impl(const error_policy_type& ep = error_policy_type())
    : error_policy_(ep)
  {}

  bool register_creator(id_param_type id, functor_param_type func)
  {
    return mappings_.insert(BOOST_DEDUCED_TYPENAME map_type::value_type(id, func))
        .second != 0;
  }

  const functor_type& operator[](id_param_type id) const
  {
    BOOST_DEDUCED_TYPENAME map_type::const_iterator i = mappings_.find(id);
    if (i != mappings_.end())
    {
      return i->second;
    }
    return error_policy_.BOOST_NESTED_TEMPLATE
        on_missing<const functor_type&, id_param_type>(id);
  }

  // Unregister a creator, returning true if any creators were actually
  // unregistered
  bool unregister(id_param_type id)
  {
    return mappings_.erase(id) != 0;
  }

  // Check to see if a creator bearing a given identifier already exists
  bool registered(id_param_type id) const
  {
    return mappings_.find(id) != mappings_.end();
  }
};

template <
    typename Signature
  , typename A1
  , typename A2
  , typename A3
>
struct factory_select_impl
{
  typedef BOOST_DEDUCED_TYPENAME factory_signature_t::bind<A1, A2, A3>::type args;

  typedef BOOST_DEDUCED_TYPENAME parameter::binding<
    args, tag::id_type, std::string
  >::type id_type;

  typedef BOOST_DEDUCED_TYPENAME parameter::binding<
    args, tag::map_type, std_map_adapter
  >::type map_type;

  typedef BOOST_DEDUCED_TYPENAME parameter::binding<
    args, tag::error_policy, exception_on_missing_id
  >::type error_policy;

  typedef factory_impl<Signature, id_type, map_type, error_policy> type;
};

} // namespace detail

// Due to Boost.Parameter not handling function signature parameters, make it a
// requirement that the first parameter to the factory always be the signature
template<
    typename Signature
  , typename A1 = parameter::void_
  , typename A2 = parameter::void_
  , typename A3 = parameter::void_
>
class factory
  : public detail::factory_select_impl<Signature, A1, A2, A3>::type
{
private:
  typedef BOOST_DEDUCED_TYPENAME detail::factory_select_impl<
    Signature, A1, A2, A3
  >::type parent_type;
public:
  typedef BOOST_DEDUCED_TYPENAME parent_type::error_policy_type error_policy_type;

  factory(const error_policy_type& ep = error_policy_type())
    : parent_type(ep) {}
};

} // namespace factory
} // namespace boost

#endif // BOOST_FACTORY_FACTORY_HPP
