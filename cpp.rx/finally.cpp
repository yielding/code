#define CATCH_CONFIG_MAIN

#include "catch.hpp"
#include "rxcpp/rx.hpp"

namespace rx = rxcpp;

SCENARIO("finally(do) sample") {
  auto v = rx::observable<>::range(1, 3)
               .finally([] { printf("The final action\n"); });
  
  v.subscribe(
      [](int v) { printf("OnNext:%d\n", v); },
      []        { printf("OnCompleted\n"); }
      );
}
