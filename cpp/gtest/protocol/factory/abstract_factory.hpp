// Copyright Tor Brede Vekterli 2008-2009 (vekterli@arcticinteractive.com)
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

// Adaptation of Andrei Alexandrescu's abstract factory pattern, as outlined in
// Modern C++ Design, but with somewhat less elegance
#ifndef BOOST_FACTORY_ABSTRACT_FACTORY_HPP
#define BOOST_FACTORY_ABSTRACT_FACTORY_HPP

#include <cstddef>
#include <boost/config.hpp>
#include <boost/mpl/inherit_linearly.hpp>
#include <boost/mpl/inherit.hpp>
#include <boost/mpl/vector.hpp>
#include <boost/mpl/map.hpp>
#include <boost/mpl/at.hpp>
#include <boost/mpl/next_prior.hpp>
#include <boost/mpl/deref.hpp>
#include <boost/mpl/front.hpp>
#include <boost/mpl/assert.hpp>
#include <boost/mpl/size.hpp>
#include <boost/mpl/equal_to.hpp>
#include <boost/mpl/zip_view.hpp>
#include <boost/mpl/if.hpp>
#include <boost/mpl/pair.hpp>
#include <boost/mpl/transform.hpp>
#include <boost/mpl/copy_if.hpp>
#include <boost/mpl/back_inserter.hpp>
#include <boost/mpl/apply.hpp>
#include <boost/mpl/fold.hpp>
#include <boost/mpl/empty_base.hpp>
#include <boost/assert.hpp>
#include <boost/type_traits/remove_pointer.hpp>
#include <boost/type_traits/remove_reference.hpp>
#include <boost/type_traits/is_function.hpp>
#include <boost/type_traits/is_pointer.hpp>
#include <boost/type_traits/is_same.hpp>
#include <boost/type_traits/function_traits.hpp>
#include <boost/ref.hpp>
#include <boost/fusion/sequence.hpp>
#include <boost/fusion/include/vector.hpp>
#include <boost/fusion/functional/invocation.hpp>
#include <boost/preprocessor/repetition.hpp>
#include <boost/preprocessor/cat.hpp>
#include <boost/preprocessor/iteration/iterate.hpp>
#include <boost/preprocessor/iteration/local.hpp>
#include "construct.hpp"

#ifndef BOOST_ABSTRACT_FACTORY_MAX_PARAMS
#  define BOOST_ABSTRACT_FACTORY_MAX_PARAMS 10
#endif
// Raising this number is currently bound to fail unless one also raises the
// maximum arity for Fusion and probably several other libraries
#ifndef BOOST_ABSTRACT_FACTORY_CREATE_MAX_ARITY
#  define BOOST_ABSTRACT_FACTORY_CREATE_MAX_ARITY 6
#endif

namespace boost { namespace factory {

namespace detail
{

template <typename T>
struct type_tag
{
  typedef T type;
};

// Simply to nudge the compiler into showing the actual sequence type
template <typename T>
struct assertion_helper
{
};

struct lazy_function_result
{
  template <typename T>
  struct apply
  {
    typedef BOOST_DEDUCED_TYPENAME boost::function_traits<T>::result_type type;
  };
};

template <typename T>
struct fuse_ref
{
  typedef boost::reference_wrapper<
    BOOST_DEDUCED_TYPENAME boost::remove_reference<T>::type
  > type;
};

// Types given to an abstract factory (and to its creation functions etc) may
// be "regular" types OR functions (that is, function types, NOT function
// pointers). If a function type is given, its result-type MUST be given as a
// pointer. This is due to the compiler otherwise instantiating the abstract
// base classes and whining about pure virtual methods (which usually will be
// there--this is an abstract factory after all)
template <typename T>
struct abstract_type_deduce
  : boost::remove_pointer<
      BOOST_DEDUCED_TYPENAME mpl::apply<
          BOOST_DEDUCED_TYPENAME mpl::if_<
              boost::is_function<T>
            , lazy_function_result
            , mpl::_1
          >::type
        , T
      >::type
    >
{};

struct na {};

struct default_base {/*TODO*/};

// Normalizes (filters) a given MPL input sequence that may contain one or more
// instances of the "na"-type into one where these are all removed, OR if the
// first element of the raw sequence is itself an MPL sequence, that sequence
// will be returned verbatim and any subsequent elements ignored.
// RawSequence must contain at least one element.
template <typename RawSequence>
struct normalize_sequence
  : mpl::if_<
        mpl::is_sequence<BOOST_DEDUCED_TYPENAME mpl::front<RawSequence>::type>
      , BOOST_DEDUCED_TYPENAME mpl::front<RawSequence>::type
      , BOOST_DEDUCED_TYPENAME mpl::copy_if<
            RawSequence
          , mpl::not_<boost::is_same<mpl::_1, na> >
          , mpl::back_inserter<mpl::vector<> >
        >::type
    >
{};

} // namespace detail

// This field is used whenever no function signature is given (or if the
// function arity specializations are insufficient, in which case a compile-
// time MPL error will be given)
template <typename T>
class abstract_factory_field
{
public:
  typedef BOOST_DEDUCED_TYPENAME boost::remove_pointer<T>::type abstract_type;
protected:
  typedef mpl::vector<> args_type;
  typedef mpl::int_<0> arity;

  typedef boost::fusion::vector<> fusion_seq_type;
public: // FIXME! VC++ bugs out here
  virtual abstract_type* do_create(
      detail::type_tag<abstract_type>, const fusion_seq_type&) const = 0;
public:
  virtual ~abstract_factory_field() {}

  // Have a public creation method here for convenience in case the factory is
  // passed as a single-type abstract factory field. This method will be hidden
  // by any 0-ary subclasses, so it shouldn't cause any ambiguousness
  abstract_type* create() const
  {
    return do_create(detail::type_tag<abstract_type>(), fusion_seq_type());
  }

  // If you've gotten this error it means you've got a higher number of function
  // parameters than BOOST_ABSTRACT_FACTORY_CREATE_MAX_ARITY is set to.
  // Define BOOST_ABSTRACT_FACTORY_CREATE_MAX_ARITY to a desired number
  // before including any abstract factory-related headers and then try re-
  // compiling.
  BOOST_MPL_ASSERT_MSG((boost::is_function<T>::value == false),
      BOOST_ABSTRACT_FACTORY_FUNCTION_ARITY_SET_TOO_LOW, (T*));
};

#define BOOST_PP_ITERATION_LIMITS (0, BOOST_ABSTRACT_FACTORY_CREATE_MAX_ARITY)
#define BOOST_PP_FILENAME_1 "factory/detail/abstract_field_n.hpp"
#include BOOST_PP_ITERATE()
/*#endif*/

#define BOOST_ABSTRACT_FACTORY_arg(z, n, na_type) \
  typename BOOST_PP_CAT(T,n) = na_type

// Note: despite the name, this actually generates a scatter-hierarchy, which
// is necessary if we want to be able to independently pass factory fields to
// functions et al without caring about its other fields. For an actual linear
// hierarchy generation, see concrete_factory_hierarchy.
// The in-memory size of an abstract_factory instance (or rather, of a
// concrete_factory instance, as abstract_factory is, unsurprisingly, abstract)
// is that of n vtable pointers, where n is the number of produceable types
// (i.e. each additional type adds another vtable, due to the scattered
// hierarchy approach) and assuming that the concrete factory holds no explicit
// state of its own
template <typename TS
  BOOST_PP_ENUM_TRAILING(
      BOOST_PP_SUB(BOOST_ABSTRACT_FACTORY_MAX_PARAMS, 1)
    , BOOST_ABSTRACT_FACTORY_arg
    , detail::na
  )>
class abstract_factory
  : public mpl::inherit_linearly<
      BOOST_DEDUCED_TYPENAME detail::normalize_sequence<
        mpl::vector<TS BOOST_PP_ENUM_TRAILING_PARAMS(
          BOOST_PP_SUB(BOOST_ABSTRACT_FACTORY_MAX_PARAMS, 1), T)>
      >::type
    , mpl::inherit<mpl::_1, abstract_factory_field<mpl::_2> >
    , mpl::empty_base/*boost::noncopyable*/
  >::type
{
public:
  typedef BOOST_DEDUCED_TYPENAME detail::normalize_sequence<
    mpl::vector<TS BOOST_PP_ENUM_TRAILING_PARAMS(
      BOOST_PP_SUB(BOOST_ABSTRACT_FACTORY_MAX_PARAMS, 1), T)>
  >::type sequence_type;

  // Create a type-mapping from "base" abstract type -> full abstract type.
  // i.e. if a sequence element is foo*(int,long), the mapping will be
  // foo -> foo*(int,long)
  typedef BOOST_DEDUCED_TYPENAME mpl::fold<
      sequence_type
    , mpl::map<>
    , mpl::insert<
        mpl::_1
      , mpl::pair<
          detail::abstract_type_deduce<mpl::_2>
        , mpl::_2
      >
    >
  >::type type_map_type;

private:
  template <typename Abstract, std::size_t N>
  struct argtype_lookup
  {
    typedef BOOST_DEDUCED_TYPENAME mpl::at<
        BOOST_DEDUCED_TYPENAME abstract_factory_field<
          BOOST_DEDUCED_TYPENAME mpl::at<type_map_type, Abstract>::type
        >::args_type
      , mpl::int_<N>
    >::type type;
  };
public:

#define BOOST_FACTORY_ABSTRACT_create_arg(z, n, unused) \
  BOOST_DEDUCED_TYPENAME argtype_lookup<BaseAbstract, n>::type BOOST_PP_CAT(arg, n)

#define BOOST_FACTORY_ABSTRACT_create_fusion_arg(z, n, varname) \
  BOOST_DEDUCED_TYPENAME field_t::fusion_arg ## n ## _type(varname ## n)

  // Lookup the type of the argument given in the function signature (if
  // present), and use that instead of templating on all args
#define BOOST_FACTORY_ABSTRACT_create(z, n, unused) \
  template <typename BaseAbstract> \
  BaseAbstract* \
    create(BOOST_PP_ENUM(n, BOOST_FACTORY_ABSTRACT_create_arg, ~)) const \
  { \
    typedef BOOST_DEDUCED_TYPENAME mpl::at<type_map_type, BaseAbstract>::type full_t; \
    typedef abstract_factory_field<full_t> field_t; \
    const field_t& field = *this; \
    return field.do_create(detail::type_tag<BaseAbstract>(), \
        BOOST_DEDUCED_TYPENAME field_t::fusion_seq_type( \
          BOOST_PP_ENUM(n, BOOST_FACTORY_ABSTRACT_create_fusion_arg, arg))); \
  }

#define BOOST_PP_LOCAL_MACRO(n) BOOST_FACTORY_ABSTRACT_create(~, n, ~)
#define BOOST_PP_LOCAL_LIMITS (0, BOOST_ABSTRACT_FACTORY_CREATE_MAX_ARITY)
#include BOOST_PP_LOCAL_ITERATE()

#undef BOOST_FACTORY_ABSTRACT_create_fusion_arg
#undef BOOST_FACTORY_ABSTRACT_create_arg
#undef BOOST_FACTORY_ABSTRACT_create
};

namespace detail {

// Super is the superclass from which the class must publicly inherit
// Due to the generic argument pack, this class works for all call-arities
template <typename ConcreteType, typename AbstractType, typename Super>
class operator_new_creator_impl
  : public Super
{
private:
  typedef BOOST_DEDUCED_TYPENAME boost::remove_pointer<
      ConcreteType>::type concrete_type;
  typedef abstract_factory_field<AbstractType> abstract_field_type;
  typedef BOOST_DEDUCED_TYPENAME abstract_field_type::abstract_type abstract_type;
  typedef BOOST_DEDUCED_TYPENAME abstract_field_type::fusion_seq_type fusion_seq_type;

  virtual abstract_type* do_create(type_tag<abstract_type>,
      const fusion_seq_type& seq) const
  {
    return boost::fusion::invoke_function_object(new_ptr<concrete_type>(), seq);
  };
public:
  virtual ~operator_new_creator_impl() {}
};

// Default match; default creator used. NOTE: default creator MUST be an MPL
// metafunction. Non-default creators (i.e. explicit such as prototype<>) need
// not be MPL metafuncs. TODO: evaluate the design of this. Might be unintuitive
// TODO2: move this out of the detail namespace?
template <
    typename ConcreteType
  , typename AbstractType
  , typename DefaultCreator
  , typename Super
>
struct select_concrete_superclass
{
  typedef BOOST_DEDUCED_TYPENAME mpl::apply<
    DefaultCreator, ConcreteType, AbstractType, Super
  >::type type;
};

// When we're generating the actual concrete factory hierarchy, it must be
// linear rather than scattered. The matching select_concrete_superclass
// must provide a type that can take a superclass type parameter
template <
    typename ConcreteSequence
  , typename AbstractSequence
  , typename DefaultCreator
  , typename Root
>
class concrete_factory_hierarchy
  : public mpl::inherit_linearly<
        mpl::zip_view< mpl::vector<ConcreteSequence, AbstractSequence> >
      , select_concrete_superclass<
          mpl::at<mpl::_2, mpl::int_<0> >
        , mpl::at<mpl::_2, mpl::int_<1> >
        , DefaultCreator
        , mpl::_1
      >
      , Root
    >
{
};

} // namespace detail

struct operator_new_creator
{
  template <typename ConcreteType, typename AbstractType, typename Super>
  struct apply
  {
    typedef detail::operator_new_creator_impl<
      ConcreteType, AbstractType, Super
    > type;
  };
};

template <
    class AbstractFactory
  , BOOST_PP_ENUM(
      BOOST_ABSTRACT_FACTORY_MAX_PARAMS
    , BOOST_ABSTRACT_FACTORY_arg
    , detail::na
)>
class concrete_factory
  : public detail::concrete_factory_hierarchy<
        BOOST_DEDUCED_TYPENAME detail::normalize_sequence<
          mpl::vector<BOOST_PP_ENUM_PARAMS(
            BOOST_ABSTRACT_FACTORY_MAX_PARAMS, T)>
        >::type
      , BOOST_DEDUCED_TYPENAME AbstractFactory::sequence_type
      , operator_new_creator // TODO: make configurable
      , AbstractFactory
    >::type
{
  typedef BOOST_DEDUCED_TYPENAME detail::normalize_sequence<
    mpl::vector<BOOST_PP_ENUM_PARAMS(BOOST_ABSTRACT_FACTORY_MAX_PARAMS, T)>
  >::type concrete_sequence_type;
  // Do a compile-time assertion to make sure both sequences are the same
  // length, as it's possible for the compile to succeed even though this
  // isn't the case (with undefined results)
  BOOST_MPL_ASSERT_MSG(
      (mpl::equal_to<
          mpl::size<BOOST_DEDUCED_TYPENAME AbstractFactory::sequence_type>
        , mpl::size<concrete_sequence_type>
      >::value)
    , ABSTRACT_AND_CONCRETE_SEQUENCE_SIZE_MISMATCH
    , (detail::assertion_helper<
        BOOST_DEDUCED_TYPENAME AbstractFactory::sequence_type>
      , concrete_sequence_type));
};

} // namespace factory
} // namespace boost

#endif // BOOST_FACTORY_ABSTRACT_FACTORY_HPP
