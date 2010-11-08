// Copyright Tor Brede Vekterli 2008-2009 (vekterli@arcticinteractive.com)
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

#ifndef BOOST_FACTORY_PROTOTYPE_HPP
#define BOOST_FACTORY_PROTOTYPE_HPP

#include <boost/config.hpp>
#include <boost/mpl/transform.hpp>
#include <boost/mpl/always.hpp>
#include <boost/shared_ptr.hpp>
#include "abstract_factory.hpp"

namespace boost { namespace factory
{

// Policies for handling prototype cloning and error handling

struct invoke_clone_function
{
  template <typename Abstract>
  static Abstract* create(const Abstract& obj)
  {
    return obj.clone();
  }
};

struct exception_on_missing_prototype
{
  template <typename Abstract>
  static Abstract* on_missing()
  {
    throw std::runtime_error("Missing factory prototype object");
  }
};

struct nullptr_on_missing_prototype
{
  template <typename Abstract>
  static Abstract* on_missing()
  {
    return 0;
  }
};

namespace detail
{

template <typename Abstract>
class prototype_container
{
  typedef BOOST_DEDUCED_TYPENAME
      abstract_type_deduce<Abstract>::type abstract_type;
  typedef boost::shared_ptr<abstract_type> pointer_type;
private:
  pointer_type proto_;

protected:
  prototype_container() {}
  ~prototype_container() {}

public:
  const pointer_type& do_get_prototype(type_tag<Abstract>) const
  {
    return proto_;
  }

  void do_set_prototype(type_tag<Abstract>,
      const pointer_type& proto)
  {
    proto_ = proto;
  }
};

// Create elements by cloning a prototype-object. The object's class must
// expose a clone() method for this to work, and the object must be set at
// runtime.
// Rather than use the indirection through a non-member friend function
// strategy to overcome name hiding issues, we pull the same trick as with
// the abstract factory itself, i.e. using a separate class that we know
// the type of and casting *this to it (hey, it's not like the auto-generated
// class-hierarchy doesn't look like horror already anyway, and there are no
// dreaded diamond multiple inheritance scenarios to deal with)
template <
    typename AbstractType
  , typename CreatorPolicy
  , typename ErrorPolicy
  , typename Super
>
class prototype_creator_impl
  : public Super
  , public prototype_container<AbstractType>
{
private:
  typedef abstract_factory_field<AbstractType> abstract_field_type;
  typedef BOOST_DEDUCED_TYPENAME abstract_field_type::abstract_type abstract_type;

  typedef BOOST_DEDUCED_TYPENAME abstract_field_type::fusion_seq_type fusion_seq_type;
  typedef CreatorPolicy creator_policy_type;
  typedef ErrorPolicy error_policy_type;

  typedef boost::shared_ptr<abstract_type> pointer_type;

  // For prototypes, the packed arguments are completely ignored
  virtual abstract_type* do_create(type_tag<abstract_type>,
      const fusion_seq_type&) const
  {
    const prototype_container<AbstractType>& pc = *this;
    const pointer_type& proto = pc.do_get_prototype(type_tag<AbstractType>());
    if (!proto.get())
      return error_policy_type::BOOST_NESTED_TEMPLATE on_missing<abstract_type>();

    return creator_policy_type::BOOST_NESTED_TEMPLATE create<abstract_type>(*proto);
  };

protected:
  virtual ~prototype_creator_impl() {}

public:
  // Since both these methods will be hidden by each subsequent prototype
  // factory subclass, everything must be done without relying on class-
  // specific typedefs such as abstract_type, i.e. everything must be templated!
  // Typedefs in the abstract factory can be referenced through Super, however.
  template <typename BaseAbstract>
  void prototype(boost::shared_ptr<BaseAbstract>& ptr)
  {
    /*typedef mpl::has_key<typename Super::type_map_type, BaseAbstract> has_key_t;
    BOOST_MPL_ASSERT_MSG(
        typename has_key_t::value
      , UNKNOWN_UNDECORATED_ABSTRACT_BASE_TYPE
      , (BaseAbstract*));*/
    // Resolve full abstract type from undecorated base type
    typedef BOOST_DEDUCED_TYPENAME mpl::at<
      BOOST_DEDUCED_TYPENAME Super::type_map_type, BaseAbstract
    >::type full_base_t;
    prototype_container<full_base_t>& pc = *this;
    pc.do_set_prototype(type_tag<full_base_t>(), ptr);
  }

  // Raw pointer overload. NOTE: Takes ownership of the pointer!
  template <typename BaseAbstract>
  void prototype(BaseAbstract* ptr)
  {
    boost::shared_ptr<BaseAbstract> sp(ptr);
    prototype<BaseAbstract>(sp);
  }

  template <typename BaseAbstract>
  void prototype(std::auto_ptr<BaseAbstract> ptr)
  {
    boost::shared_ptr<BaseAbstract> sp(ptr);
    prototype<BaseAbstract>(sp);
  }

  template <typename BaseAbstract>
  const boost::shared_ptr<BaseAbstract>& prototype() const
  {
    /*BOOST_MPL_ASSERT_MSG(
        (mpl::has_key<type_map_type, BaseAbstract>)
      , UNKNOWN_UNDECORATED_ABSTRACT_BASE_TYPE
      , (BaseAbstract));*/
    // Resolve full abstract type from undecorated base type
    typedef BOOST_DEDUCED_TYPENAME mpl::at<
      BOOST_DEDUCED_TYPENAME Super::type_map_type, BaseAbstract
    >::type full_base_t;
    const prototype_container<full_base_t>& pc = *this;
    return pc.do_get_prototype(type_tag<full_base_t>());
  }
};

} // namespace detail

template <
    class CreatorPolicy = invoke_clone_function
  , class ErrorPolicy   = exception_on_missing_prototype
>
struct prototype
{
  typedef CreatorPolicy creator_policy_type;
  typedef ErrorPolicy error_policy_type;

  /* Having prototype<> default-able doesn't really make all that much sense..?
  template <typename ConcreteType, typename AbstractType, typename Super>
  struct apply
  {
    typedef detail::prototype_creator_impl<
      AbstractType, CreatorPolicy, ErrorPolicy, Super
    > type;
  };*/
};

namespace detail {

// match on prototype<...>
template <
    typename CreatorPolicy, typename ErrorPolicy
  , typename AbstractType, typename DefaultCreator
  , typename Super
>
struct select_concrete_superclass<
    prototype<CreatorPolicy, ErrorPolicy>, AbstractType, DefaultCreator, Super
>
{
  typedef prototype_creator_impl<AbstractType, CreatorPolicy,
      ErrorPolicy, Super> type;
};

} // namespace detail

// Simple wrapper around concrete_factory that transforms the n-element
// abstract sequence into an n-element prototype<> concrete sequence
template <
    class AbstractFactory
  , class CreatorPolicy = invoke_clone_function
  , class ErrorPolicy   = exception_on_missing_prototype
>
class prototype_factory
  : public concrete_factory<
        AbstractFactory
      , BOOST_DEDUCED_TYPENAME mpl::transform<
            BOOST_DEDUCED_TYPENAME AbstractFactory::sequence_type
          , mpl::always< prototype<CreatorPolicy, ErrorPolicy> >
        >::type
  >
{
};

} // namespace factory
} // namespace boost

#endif // BOOST_FACTORY_PROTOTYPE_HPP
