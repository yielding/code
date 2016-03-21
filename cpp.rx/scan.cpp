#define CATCH_CONFIG_MAIN

#include "rxcpp/rx.hpp"
#include "catch.hpp"

SCENARIO("scan sample") {
  auto v = rxcpp::observable<>::range(1, 10)
    .scan(0, [](int s, int v) { return s + v; });

  v.subscribe(
      [](int v) { printf("OnNext: %d\n", v); },
      [] { printf("Completed\n"); });
}
