// Copyright Tor Brede Vekterli 2008-2009 (vekterli@arcticinteractive.com)
// Distributed under the Boost Software License, Version 1.0.
//    (See accompanying file LICENSE_1_0.txt or copy at
//          http://www.boost.org/LICENSE_1_0.txt)

// Boost.Preprocessor template file
#ifndef BOOST_PP_IS_ITERATING
#  error "Do not manually include this header template file"
#endif

#define n BOOST_PP_ITERATION()

#define BOOST_ABSTRACT_FACTORY_FIELD_fusion_arg(z, n, unused) \
  typedef BOOST_DEDUCED_TYPENAME detail::fuse_ref<A ## n>::type fusion_arg ## n ## _type;

#define BOOST_ABSTRACT_FACTORY_FIELD_fusion_seq_arg(z, n, unused) \
  fusion_arg ## n ## _type

#define BOOST_ABSTRACT_FACTORY_FIELD_fusion_wrap_ref(z, n, varname) \
  fusion_arg ## n ## _type(varname ## n)

template <
    typename T
    BOOST_PP_ENUM_TRAILING_PARAMS(n, typename A)
>
class abstract_factory_field<T(BOOST_PP_ENUM_PARAMS(n, A))>
{
public:
  typedef BOOST_DEDUCED_TYPENAME boost::remove_pointer<T>::type abstract_type;
protected:
  BOOST_PP_REPEAT(n, BOOST_ABSTRACT_FACTORY_FIELD_fusion_arg, ~)

  typedef mpl::vector<BOOST_PP_ENUM_PARAMS(n, A)> args_type;

  typedef mpl::int_<n> arity;

  typedef boost::fusion::vector<
    BOOST_PP_ENUM(n, BOOST_ABSTRACT_FACTORY_FIELD_fusion_seq_arg, ~)
  > fusion_seq_type;

public: // FIXME! VC++ complains otherwise
  virtual abstract_type* do_create(detail::type_tag<abstract_type>,
      const fusion_seq_type&) const = 0;

public:
  virtual ~abstract_factory_field() {}

  abstract_type* create(BOOST_PP_ENUM_BINARY_PARAMS(n, A, arg)) const
  {
    return do_create(detail::type_tag<abstract_type>(), fusion_seq_type(
      BOOST_PP_ENUM(n, BOOST_ABSTRACT_FACTORY_FIELD_fusion_wrap_ref, arg)
    ));
  }
};


#undef BOOST_ABSTRACT_FACTORY_FIELD_fusion_wrap_ref
#undef BOOST_ABSTRACT_FACTORY_FIELD_fusion_arg
#undef BOOST_ABSTRACT_FACTORY_FIELD_fusion_seq_arg

#undef n
