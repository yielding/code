#pragma once

#include <utility>
#include <functional>
#include <type_traits>
#include <optional>
#include <expected>

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
namespace monad_pipe
{
  using namespace std;

  template<class M, class F>
  constexpr auto operator|(M&& m, F&& f)
    noexcept(noexcept(std::forward<F>(f)(std::forward<M>(m))))
    -> decltype(std::forward<F>(f)(std::forward<M>(m)))
  {
    return std::forward<F>(f)(std::forward<M>(m));
  }

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
namespace detail 
{
  template<class Opt, class F>
  auto optional_and_then(Opt&& o, F&& f) -> auto
  {
    using O = remove_reference_t<Opt>;
    if (o) 
      return std::invoke(std::forward<F>(f), *std::forward<Opt>(o));

    if constexpr (is_const_v<O>) 
      return optional<invoke_result_t<F, const typename O::value_type&>>{};

    return optional<invoke_result_t<F, typename O::value_type&>>{};
  }

  template<class Opt, class F>
  auto optional_transform(Opt&& o, F&& f) -> auto
  {
    using O = remove_reference_t<Opt>;
    using V = typename O::value_type;
    using R = invoke_result_t<F, conditional_t<is_const_v<O>, const V&, V&>>;
    if (o) 
      return optional<R>{ std::invoke(std::forward<F>(f), *std::forward<Opt>(o)) };

    return optional<R>{};
  }
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
  struct and_then_t 
  {
    template<class F>
    struct fn 
    {
      F _f;
      
      template<class M>
      auto operator()(M&& m) const -> auto
      {
        if constexpr (requires { std::forward<M>(m).and_then(_f); }) 
          return std::forward<M>(m).and_then(_f);
        else if constexpr (is_same_v<remove_cv_t<remove_reference_t<M>>,
                             optional<typename remove_cv_t<remove_reference_t<M>>::value_type>>)
          return detail::optional_and_then(std::forward<M>(m), _f);
        else 
          static_assert(sizeof(M) == 0, "and_then: unsupported monad type");
      }
    };
    
    template<class F>
    auto operator()(F&& f) const -> fn<decay_t<F>> { return { std::forward<F>(f) }; }
  } inline constexpr and_then{};

  struct transform_t 
  {
    template<class F>
    struct fn 
    {
      F _f;
      
      template<class M>
      auto operator()(M&& m) const -> auto
      {
        if constexpr (requires { std::forward<M>(m).transform(_f); }) {
          return std::forward<M>(m).transform(_f);
        } else if constexpr (is_same_v<remove_cv_t<remove_reference_t<M>>,
                             optional<typename remove_cv_t<remove_reference_t<M>>::value_type>>) {
          return detail::optional_transform(std::forward<M>(m), _f);
        } else {
          static_assert(sizeof(M) == 0, "transform: unsupported monad type");
        }
      }
    };
    
    template<class F>
    auto operator()(F&& f) const -> fn<decay_t<F>> { return { std::forward<F>(f) }; }
  } inline constexpr transform{};

  struct or_else_t 
  {
    template<class F>
    struct fn 
    {
      F _f;
      
      template<class M>
      auto operator()(M&& m) const -> auto
      {
        if constexpr (requires { std::forward<M>(m).or_else(_f); }) {
          return std::forward<M>(m).or_else(_f);
        } else {
          return std::forward<M>(m);
        }
      }
    };
    
    template<class F>
    auto operator()(F&& f) const -> fn<decay_t<F>> { return { std::forward<F>(f) }; }
  } inline constexpr or_else{};

  struct tap_t 
  {
    template<class F>
    struct fn 
    {
      F _f;
      
      template<class Opt>
      auto operator()(Opt&& o) const -> Opt&& 
      {
        if (o) std::invoke(_f, *o);
        return std::forward<Opt>(o);
      }
    };
    
    template<class F>
    auto operator()(F&& f) const -> fn<decay_t<F>> { return { std::forward<F>(f) }; }
  } inline constexpr tap{};

  struct tap_value_t 
  {
    template<class F>
    struct fn 
    {
      F _f;
      
      template<class Ex>
      auto operator()(Ex&& ex) const -> Ex&& 
      {
        if (ex) std::invoke(_f, *ex);
        return std::forward<Ex>(ex);
      }
    };
    
    template<class F>
    auto operator()(F&& f) const -> fn<decay_t<F>> { return { std::forward<F>(f) }; }
  } inline constexpr tap_value{};

  struct tap_error_t 
  {
    template<class F>
    struct fn 
    {
      F _f;
      
      template<class Ex>
      auto operator()(Ex&& ex) const -> Ex&& 
      {
        if (!ex) std::invoke(_f, ex.error());
        return std::forward<Ex>(ex);
      }
    };
    
    template<class F>
    auto operator()(F&& f) const -> fn<decay_t<F>> { return { std::forward<F>(f) }; }
  } inline constexpr tap_error{};
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////