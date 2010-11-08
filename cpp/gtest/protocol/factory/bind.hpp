// Copyright Tor Brede Vekterli 2008-2009 (vekterli@arcticinteractive.com)
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

#ifndef BOOST_ABSTRACT_FACTORY_BIND_HPP
#define BOOST_ABSTRACT_FACTORY_BIND_HPP

#include <boost/mpl/filter_view.hpp>
#include <boost/mpl/reverse.hpp>
#include <boost/fusion/include/at.hpp>
#include <boost/fusion/include/cons.hpp>
#include <boost/fusion/include/distance.hpp>
#include <boost/fusion/include/mpl.hpp>
#include <boost/fusion/include/fold.hpp>
#include "abstract_factory.hpp"

namespace boost { namespace factory
{

namespace placeholders
{
  // Generate placeholder types _1, _2, ..., _n, where n is maximum factory
  // param arity. Placeholder types need only be valid MPL integral constants
#define BOOST_ABSTRACT_FACTORY_placeholder(z, n, unused) \
  typedef mpl::int_<n> BOOST_PP_CAT(_, n);

#define BOOST_PP_LOCAL_MACRO(n) BOOST_ABSTRACT_FACTORY_placeholder(~, n, ~)
#define BOOST_PP_LOCAL_LIMITS (1, BOOST_ABSTRACT_FACTORY_MAX_PARAMS)
#include BOOST_PP_LOCAL_ITERATE()
#undef BOOST_ABSTRACT_FACTORY_placeholder
} // namespace placeholders

// Import placeholders to factory namespace
#define BOOST_ABSTRACT_FACTORY_placeholder_import(z, n, unused) \
  using placeholders:: BOOST_PP_CAT(_, n);

#define BOOST_PP_LOCAL_MACRO(n) BOOST_ABSTRACT_FACTORY_placeholder_import(~, n, ~)
#define BOOST_PP_LOCAL_LIMITS (1, BOOST_ABSTRACT_FACTORY_MAX_PARAMS)
#include BOOST_PP_LOCAL_ITERATE()
#undef BOOST_ABSTRACT_FACTORY_placeholder_import

namespace detail {

// RawPlaceholders is an MPL sequence with 0..n placeholders OR types of
// detail::factory::na
template <typename RawPlaceholders>
class bind_impl
{
  // TODO: don't reverse the list but rather do car/cdr list creation in the
  // proper order instead? End-effect is the same, either way
  // TODO: or just use front_inserter!
  typedef BOOST_DEDUCED_TYPENAME mpl::copy_if<
      BOOST_DEDUCED_TYPENAME mpl::reverse<RawPlaceholders>::type
    , mpl::not_< boost::is_same<mpl::_1, na> >
    , mpl::back_inserter< fusion::vector<> >
  >::type placeholders_t;

  // Effectively transforms placeholders_t into a cons-list of elements
  // from Seq where the element# fetched is that of the placeholder's value - 1
  template <typename Seq>
  struct make_bound_sequence
  {
    const Seq& seq_;

    make_bound_sequence(const Seq& s)
      : seq_(s)
    {}

    typedef fusion::result_of::distance<
        BOOST_DEDUCED_TYPENAME fusion::result_of::begin<Seq>::type
      , BOOST_DEDUCED_TYPENAME fusion::result_of::end<Seq>::type
    > seq_distance_t;

    template <typename Arg>
    struct arg_at
    {
      typedef BOOST_DEDUCED_TYPENAME mpl::minus<
          Arg
        , mpl::int_<1>
      >::type real_index;

      // If you have gotten this error it means the arity _n you have used is
      // > the maximum arity given by BOOST_ABSTRACT_FACTORY_MAX_PARAMS
      BOOST_MPL_ASSERT_MSG(
          (mpl::less<real_index, seq_distance_t>::value)
        , BIND_PLACEHOLDER_ARGUMENT_OUT_OF_BOUNDS
        , (Arg)
      );

      typedef BOOST_DEDUCED_TYPENAME fusion::result_of::at<
        Seq, real_index
      >::type type;
    };

    template <typename Sig>
    struct result;

    // Our return type is the basic LISP-esque list structure used by Fusion,
    // as it is both easy and efficient to do construction piece by piece.
    // There is probably a much more elegant way to do this, but it works
    template <typename F, typename E, typename S>
    struct result<F(E,S)>
    {
      typedef BOOST_DEDUCED_TYPENAME fusion::cons<
          BOOST_DEDUCED_TYPENAME boost::remove_reference<
            BOOST_DEDUCED_TYPENAME arg_at<
              BOOST_DEDUCED_TYPENAME boost::remove_reference<E>::type
            >::type
          >::type
        , BOOST_DEDUCED_TYPENAME boost::remove_reference<S>::type
      > type;
    };

    // Add the reference wrapper stored at the argument sequence location pointed
    // to by the placeholder. Say that quickly 3 times in a row.
    // Currently builds the list in reverse order, hence the initial reversal
    // of the placeholder list
    template <typename E, typename S>
    BOOST_DEDUCED_TYPENAME fusion::cons<
        BOOST_DEDUCED_TYPENAME boost::remove_reference<
          BOOST_DEDUCED_TYPENAME arg_at<
            BOOST_DEDUCED_TYPENAME boost::remove_reference<E>::type
          >::type
        >::type
      , BOOST_DEDUCED_TYPENAME boost::remove_reference<S>::type
    > operator()(const E&, const S& s) const
    {
      return fusion::cons<
          BOOST_DEDUCED_TYPENAME boost::remove_reference<
            BOOST_DEDUCED_TYPENAME arg_at<
              BOOST_DEDUCED_TYPENAME boost::remove_reference<E>::type
            >::type
          >::type
        , BOOST_DEDUCED_TYPENAME boost::remove_reference<S>::type
      >(fusion::at<BOOST_DEDUCED_TYPENAME arg_at<
          BOOST_DEDUCED_TYPENAME boost::remove_reference<E>::type
        >::real_index>(seq_), s);
    }
  };

public:
  template <typename R, typename F, typename Seq>
  static R invoke_with_bound_args(const F& f, const Seq& seq)
  {
    return fusion::invoke(f, fusion::fold(
      placeholders_t(), fusion::nil(), make_bound_sequence<Seq>(seq)));
  }
};

} // namespace detail

template <typename Sig>
struct bind;

namespace detail {

template <
    typename ConcreteType, typename RawPlaceholders
  , typename AbstractType, typename Super
>
class bound_creator_impl
  : public Super
{
private:
  typedef ConcreteType concrete_type;
  typedef abstract_factory_field<AbstractType> abstract_field_type;
  typedef BOOST_DEDUCED_TYPENAME abstract_field_type::abstract_type abstract_type;
  typedef BOOST_DEDUCED_TYPENAME abstract_field_type::fusion_seq_type fusion_seq_type;

  virtual abstract_type* do_create(type_tag<abstract_type>,
      const fusion_seq_type& seq) const
  {
    return bind_impl<RawPlaceholders>::BOOST_NESTED_TEMPLATE invoke_with_bound_args<abstract_type*>(
      new_ptr<concrete_type>(), seq);
  };
};

// Specializations for all bind<> arities
// TODO: use Boost.FunctionTypes instead?
#define BOOST_FACTORY_ABSTRACT_bind_select(z, n, unused) \
template < \
    typename ConcreteType BOOST_PP_ENUM_TRAILING_PARAMS(n, typename T) \
  , typename AbstractType, typename DefaultCreator, typename Super \
> \
struct select_concrete_superclass< \
    bind<ConcreteType(BOOST_PP_ENUM_PARAMS(n, T))> \
  , AbstractType, DefaultCreator, Super \
> \
{ \
  typedef bound_creator_impl<ConcreteType, mpl::vector< \
    BOOST_PP_ENUM_PARAMS(n, T) \
  >, AbstractType, Super> type; \
};

#define BOOST_PP_LOCAL_MACRO(n) BOOST_FACTORY_ABSTRACT_bind_select(~, n, ~)
#define BOOST_PP_LOCAL_LIMITS (0, BOOST_ABSTRACT_FACTORY_MAX_PARAMS)
#include BOOST_PP_LOCAL_ITERATE()

#undef BOOST_FACTORY_ABSTRACT_bind_select

} // namespace detail
} // namespace factory
} // namespace boost

#endif // BOOST_ABSTRACT_FACTORY_BIND_HPP
