// expected_util.hpp
#pragma once
#include <expected>
#include <utility>
#include <type_traits>

/*
 * expected_util.hpp (C++23)
 * ----------------------------------------
 * - 파이프 합성: ex | f  →  f(std::move(ex))
 * - 어댑터: and_then_f, transform_f, transform_error_f
 * - tee:    tee_value_f, tee_error_f, tee_both_f
 *
 * 사용 예:
 *   using Ex = std::expected<int, std::error_code>;
 *
 *   Ex step1();
 *   std::expected<int, std::error_code> step2(int);
 *
 *   auto r = step1()
 *       | tee_value_f([](int v){ log(v); })
 *       | and_then_f(step2)
 *       | tee_both_f(
 *            [](int v){ log_ok(v); },
 *            [](auto const& e){ log_err(e); }
 *         )
 *       | transform_f([](int v){ return v + 1; });
 */

namespace expx {

  using namespace std;
  
  // ──────────────────────────────────────────────────────────────
  // 0) expected 파이프 프레임워크
  //    ex | f   →   f(std::move(ex))
  //    f는 호출 연산자(operator() 또는 함수 객체)로
  //    std::expected<T,E>&& 를 인자로 받아야 합니다.
  template <class T, class E, class F>
  constexpr auto operator|(expected<T, E>&& ex, F&& f)
    noexcept(noexcept(forward<F>(f)(std::move(ex)))) -> decltype(forward<F>(f)(std::move(ex))) 
  {
    return forward<F>(f)(std::move(ex));
  }
  
  // ──────────────────────────────────────────────────────────────
  // 1) 어댑터: and_then / transform / transform_error
  //    표준 멤버 함수를 파이프 가능하도록 래핑
  
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
    constexpr auto operator()(expected<T, E>&& ex) const noexcept(noexcept(std::move(ex).transform_error(f)))
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
  
  // ──────────────────────────────────────────────────────────────
  // 2) tee 유틸: 값/에러를 "찍고 그대로 흘려보내기"
  
  // 값 tee: 성공 시 fv(value) 호출, ex는 그대로 반환
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
  
  // 에러 tee: 실패 시 fe(error) 호출, ex는 그대로 반환
  template <class FE>
  struct TeeErrorF 
  {
    FE fe;
    template <class T, class E>
    constexpr auto operator()(expected<T, E>&& ex) const
      noexcept(noexcept(fe(ex.error())) && is_nothrow_move_constructible_v<expected<T,E>>)
      -> expected<T, E> 
    {
      if (!ex) 
        fe(ex.error());
  
      return std::move(ex);
    }
  };
  
  template <class FE>
  constexpr auto tee_error_f(FE fe) 
  {
    return TeeErrorF<FE>{std::move(fe)};
  }
  
  // 값/에러 모두 tee: 성공이면 fv(value), 실패면 fe(error)
  template <class FV, class FE>
  struct TeeBothF 
  {
    FV fv; FE fe;
    template <class T, class E>
    constexpr auto operator()(expected<T, E>&& ex) const
      noexcept((noexcept(fv(*ex)) || noexcept(fe(ex.error()))) && is_nothrow_move_constructible_v<expected<T,E>>)
      -> expected<T, E> 
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
  
  // ──────────────────────────────────────────────────────────────
  // 3) 헬퍼: 성공/실패 분기(선택 사항)
  //    expected를 받아 성공/실패 콜백 중 하나를 실행하고, 원본을 그대로 반환
  template <class FV, class FE>
  struct TapF 
  {
    FV fv; FE fe;
    template <class T, class E>
    constexpr auto operator()(expected<T,E>&& ex) const 
      noexcept(noexcept(fv(*ex)) && noexcept(fe(ex.error())))
      -> expected<T,E> 
    {
      if (ex) 
        fv(*ex); 
      else 
        fe(ex.error());
  
      return std::move(ex);
    }
  };
  
  template <class FV, class FE>
  constexpr auto tap_f(FV fv, FE fe) 
  { 
    return TapF<FV,FE>{std::move(fv), std::move(fe)}; 
  }

} // namespace expx
