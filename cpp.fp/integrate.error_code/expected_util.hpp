#pragma once

#include <expected>
#include <utility>
#include <type_traits>

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
namespace expx
{
  using namespace std;

  template <class T, class E, class F>
  constexpr auto operator|(expected<T, E>&& ex, F&& f) noexcept(noexcept(std::forward<F>(f)(std::move(ex))))
    -> decltype(std::forward<F>(f)(std::move(ex))) 
  {
    return std::forward<F>(f)(std::move(ex));
  }

  template <class F>
  struct AndThenF 
  {
    F f;

    template <class T, class E>
    constexpr auto operator()(expected<T, E>&& ex) const noexcept(noexcept(std::move(ex).and_then(f)))
      -> decltype(std::move(ex).and_then(f)) 
    {
      return std::move(ex).and_then(f);
    }
  };

  template <class F>
  constexpr auto and_then_f(F f) 
  { 
    return AndThenF<F>{std::move(f)}; 
  }

  template <class F>
  struct TransformF 
  {
    F f;

    template <class T, class E>
    constexpr auto operator()(expected<T, E>&& ex) const noexcept(noexcept(std::move(ex).transform(f)))
      -> decltype(std::move(ex).transform(f)) 
    {
      return std::move(ex).transform(f);
    }
  };

  template <class F>
  constexpr auto transform_f(F f) 
  { 
    return TransformF<F>{std::move(f)}; 
  }

  template <class F>
  struct TransformErrF 
  {
    F f;

    template <class T, class E>
    constexpr auto operator()(expected<T, E>&& ex) const
      noexcept(noexcept(std::move(ex).transform_error(f)))
      -> decltype(std::move(ex).transform_error(f)) 
    {
      return std::move(ex).transform_error(f);
    }
  };

  template <class F>
  constexpr auto transform_error_f(F f) 
  { 
    return TransformErrF<F>{std::move(f)}; 
  }

  template <class FV>
  struct TeeValueF 
  {
    FV fv;

    template <class T, class E>
    constexpr auto operator()(expected<T, E>&& ex) const noexcept(noexcept(fv(*ex)) && is_nothrow_move_constructible_v<expected<T,E>>)
      -> expected<T, E> 
    {
      if (ex) fv(*ex);
      return std::move(ex);
    }
  };

  template <class FV>
  constexpr auto tee_value_f(FV fv) 
  { 
    return TeeValueF<FV>{std::move(fv)}; 
  }

  template <class FE>
  struct TeeErrorF 
  {
    FE fe;
    template <class T, class E>
    constexpr auto operator()(expected<T, E>&& ex) const noexcept(noexcept(fe(ex.error())) && is_nothrow_move_constructible_v<expected<T,E>>)
      -> expected<T, E> 
    {
      if (!ex) fe(ex.error());
      return std::move(ex);
    }
  };

  template <class FE>
  constexpr auto tee_error_f(FE fe) 
  { 
    return TeeErrorF<FE>{std::move(fe)}; 
  }

  template <class FV, class FE>
  struct TeeBothF 
  {
    FV fv; FE fe;
    template <class T, class E>
    constexpr auto operator()(expected<T, E>&& ex) const
      noexcept((noexcept(fv(*ex)) || noexcept(fe(ex.error()))) &&
        is_nothrow_move_constructible_v<expected<T,E>>
      ) -> expected<T, E> 
    {
      if (ex) 
        fv(*ex);
      else 
        fe(ex.error());

      return std::move(ex);
    }
  };

  template <class FV, class FE>
  constexpr auto tee_both_f(FV fv, FE fe) 
  { 
    return TeeBothF<FV, FE>{std::move(fv), std::move(fe)}; 
  }
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
