#include "expected_util.hpp"

#include <expected>
#include <iostream>
#include <print>
#include <string>
#include <system_error>

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
namespace tee_example
{
  using namespace std;
  using namespace expx;

  using Error = error_code;

  template <class T>
  using result = expected<T, Error>;

  //////////////////////////////////////////////////////////////////////////////
  //
  //
  //
  //////////////////////////////////////////////////////////////////////////////
  template <typename F>
  auto tee(F f)
  {
    return [f = move(f)](auto&& v) {
      f(v);
      return forward<decltype(v)>(v);
    };
  }

  auto step(const int v) -> expected<int, string>
  {
    if (v == 10)
      return unexpected{"error"};

    return v + 1;
  }

  auto step1(const bool ok) -> result<int> 
  {
    if (ok) return 7;
    return unexpected{make_error_code(errc::operation_canceled)};
  }

  auto step2(const int v) -> result<int> 
  {
    if (v % 2 == 1) return v * 100;

    return unexpected{make_error_code(errc::invalid_argument)};
  }

  //////////////////////////////////////////////////////////////////////////////
  //
  //
  //
  //////////////////////////////////////////////////////////////////////////////
  auto main() -> int 
  {
    auto ok = step1(true)
      | tee_value_f([](const int v) { println("[tee value] step1={}", v); })
      | and_then_f(step2)
      | tee_both_f(
          [](const auto& v) { println("[tee both] value={}", v); },
          [](const auto& e) { println("[tee both] error={}", e.message()); }
        )
      | transform_f([](const int v) { return v + 1; });

    ok ? println("OK final={}\n", *ok)
       : println("OK error={}\n", ok.error().message());

    auto bad = step1(false)
      | tee_error_f([](const auto& e){ println("[tee error] {}", e.message()); })
      | and_then_f(step2)
      | transform_error_f([](const auto e) { return e; });

    bad ? println("BAD final={}", *bad)
        : println("BAD error={}", bad.error().message());

    return 0;
  }
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
int main() 
{
  return tee_example::main();
}
