// Copyright Tor Brede Vekterli 2008-2009 (vekterli@arcticinteractive.com)
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

#ifndef BOOST_FACTORY_DYNAMIC_HPP
#define BOOST_FACTORY_DYNAMIC_HPP

#include <boost/config.hpp>
#include <boost/mpl/transform.hpp>
#include <boost/mpl/always.hpp>
#include <boost/function.hpp>
#include "abstract_factory.hpp"

namespace boost { namespace factory
{

struct exception_on_missing_dynamic
{
  template <typename Abstract>
  static Abstract* on_missing()
  {
    throw std::runtime_error("Missing abstract factory dynamic function");
  }
};

namespace detail
{

template <typename R>
struct add_function
{
  typedef R* type(void);
};

template <typename T>
struct resolve_function_type
  : mpl::if_<
        boost::is_function<T>
      , mpl::identity<T>
      , add_function<typename boost::remove_pointer<T>::type>
    >::type
{};

// Store a boost::function with a signature that matches that of the abstract
// base type. If the base signature is already a function signature, the job's
// done for us. However, if it's not, it must be converted to the form R*(void),
// where R is the original base type with no pointer
template <typename Abstract>
class function_container
{
  /*typedef BOOST_DEDUCED_TYPENAME
      abstract_type_deduce<Abstract>::type abstract_type;*/
protected:
  typedef BOOST_DEDUCED_TYPENAME resolve_function_type<Abstract>::type
      raw_function_type;
  typedef boost::function<raw_function_type> function_type;
private:
  function_type func_;

public:
  const function_type& do_get_function(type_tag<Abstract>) const
  {
    return func_;
  }

  void do_set_function(type_tag<Abstract>,
      const function_type& func)
  {
    func_ = func;
  }
};

// Same strategy as with prototypes
template <
    typename AbstractType
  , typename ErrorPolicy
  , typename Super
>
class dynamic_creator_impl
  : public Super
  , public function_container<AbstractType>
{
private:
  typedef abstract_factory_field<AbstractType> abstract_field_type;
  typedef BOOST_DEDUCED_TYPENAME abstract_field_type::abstract_type abstract_type;
  typedef BOOST_DEDUCED_TYPENAME abstract_field_type::fusion_seq_type fusion_seq_type;
  typedef ErrorPolicy error_policy_type;
  typedef BOOST_DEDUCED_TYPENAME function_container<
      AbstractType>::function_type function_type;

  // The stored function object is invoked with the fusion sequence's arguments
  virtual abstract_type* do_create(type_tag<abstract_type>,
      const fusion_seq_type& seq) const
  {
    const function_container<AbstractType>& pc = *this;
    const function_type& func = pc.do_get_function(type_tag<AbstractType>());
    if (!func)
      return error_policy_type::BOOST_NESTED_TEMPLATE on_missing<abstract_type>();

    return boost::fusion::invoke_function_object(func, seq);
  };

protected:
  virtual ~dynamic_creator_impl() {}

public:
  // Since both these methods will be hidden by each subsequent dynamic
  // factory subclass, everything must be done without relying on class-
  // specific typedefs such as abstract_type, i.e. everything must be templated!
  // Typedefs in the abstract factory can be referenced through Super, however.
  template <typename BaseAbstract>
  void dynamic(const boost::function<
    BOOST_DEDUCED_TYPENAME resolve_function_type<
      BOOST_DEDUCED_TYPENAME mpl::at<
        BOOST_DEDUCED_TYPENAME Super::type_map_type, BaseAbstract
      >::type
    >::type
  >& func)
  {
    // Resolve full abstract type from undecorated base type
    typedef BOOST_DEDUCED_TYPENAME mpl::at<
      BOOST_DEDUCED_TYPENAME Super::type_map_type, BaseAbstract
    >::type full_base_t;
    function_container<full_base_t>& pc = *this;
    pc.do_set_function(type_tag<full_base_t>(), func);
  }

  template <typename BaseAbstract>
  const boost::function<
    BOOST_DEDUCED_TYPENAME resolve_function_type<
      BOOST_DEDUCED_TYPENAME mpl::at<
        BOOST_DEDUCED_TYPENAME Super::type_map_type, BaseAbstract
      >::type
    >::type
  >& dynamic() const
  {
    // Resolve full abstract type from undecorated base type
    typedef BOOST_DEDUCED_TYPENAME mpl::at<
      BOOST_DEDUCED_TYPENAME Super::type_map_type, BaseAbstract
    >::type full_base_t;
    const function_container<full_base_t>& pc = *this;
    return pc.do_get_function(type_tag<full_base_t>());
  }
};

} // namespace detail

template <
  class ErrorPolicy = exception_on_missing_dynamic
>
struct dynamic
{
  typedef ErrorPolicy error_policy_type;
};

namespace detail {

// match on dynamic<...>
template <
    typename ErrorPolicy, typename AbstractType, typename DefaultCreator
  , typename Super
>
struct select_concrete_superclass<
    dynamic<ErrorPolicy>, AbstractType, DefaultCreator, Super
>
{
  typedef dynamic_creator_impl<AbstractType,
      ErrorPolicy, Super> type;
};

} // namespace detail

// Simple wrapper around concrete_factory that transforms the n-element
// abstract sequence into an n-element dynamic<> concrete sequence
template <
    class AbstractFactory
  , class ErrorPolicy = exception_on_missing_dynamic
>
class dynamic_factory
  : public concrete_factory<
        AbstractFactory
      , BOOST_DEDUCED_TYPENAME mpl::transform<
            BOOST_DEDUCED_TYPENAME AbstractFactory::sequence_type
          , mpl::always< dynamic<ErrorPolicy> >
        >::type
  >
{
};

} // namespace factory
} // namespace boost

#endif // BOOST_FACTORY_DYNAMIC_HPP
