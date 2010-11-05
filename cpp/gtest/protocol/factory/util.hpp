// Copyright Tor Brede Vekterli 2008-2009 (vekterli@arcticinteractive.com)
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

#ifndef BOOST_FACTORY_UTIL_HPP
#define BOOST_FACTORY_UTIL_HPP

#include <memory>
#include "construct.hpp"

namespace boost { namespace factory
{

template <typename T, class Factory>
bool register_wrapped_new_ptr(Factory& factory,
    BOOST_DEDUCED_TYPENAME Factory::id_param_type id)
{
  return factory.register_creator(id, wrapped_new_ptr<
      T
    , BOOST_DEDUCED_TYPENAME Factory::result_type
  >());
}

template <typename T, class Factory>
bool register_new_ptr(Factory& factory, 
    BOOST_DEDUCED_TYPENAME Factory::id_param_type id)
{
  return factory.register_creator(id, new_ptr<T>());
}

} // namespace factory
} // namespace boost

#endif // BOOST_FACTORY_UTIL_HPP
