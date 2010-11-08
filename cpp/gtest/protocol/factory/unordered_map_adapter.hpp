// Copyright Tor Brede Vekterli 2008-2009 (vekterli@arcticinteractive.com)
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

#ifndef BOOST_FACTORY_UNORDERED_MAP_ADAPTER_HPP
#define BOOST_FACTORY_UNORDERED_MAP_ADAPTER_HPP

// Boost.TR1 doesn't yet support auto-inclusion of native unordered_map if
// present, so doing it in an ugly way that hopefully will work until that
// is fixed

#include <boost/tr1/detail/config.hpp>

#ifdef BOOST_HAS_TR1_UNORDERED_MAP
#include <unordered_map>

namespace boost { namespace factory
{

struct std_unordered_map_adapter
{
  template <typename Key, class Value>
  struct apply
  {
    typedef std::tr1::unordered_map<Key, Value> type;
  };
};

} // namespace factory
} // namespace boost

// Meant for the unit testing; cannot be guaranteed to be here in future
// versions once Boost.TR1 is modified
#define BOOST_FACTORY_UNORDERED_MAP_SUPPORTED
#elif BOOST_VERSION >= 103600
// No native TR1 map, but the Boost version is >= 1.36 so use its unordered map
// instead

#include <boost/unordered_map.hpp>

namespace boost { namespace factory
{

struct std_unordered_map_adapter
{
  template <typename Key, class Value>
  struct apply
  {
    typedef boost::unordered_map<Key, Value> type;
  }
};

} // namespace factory
} // namespace boost

#define BOOST_FACTORY_UNORDERED_MAP_SUPPORTED
#endif // BOOST_VERSION >= 103600

#endif // BOOST_FACTORY_UNORDERED_MAP_ADAPTER_HPP
