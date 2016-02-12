#define CATCH_CONFIG_MAIN

#include "catch.hpp"
#include "rxcpp/rx.hpp"

using namespace std;
      namespace rx = rxcpp;

using triple = tuple<int, int, int>;

SCENARIO("combine_latest sample") {
  auto o1 = rx::observable<>::interval(chrono::milliseconds(2));
  auto o2 = rx::observable<>::interval(chrono::milliseconds(3));
  auto o3 = rx::observable<>::interval(chrono::milliseconds(5));
  auto values = o1.combine_latest(o2, o3);

  auto on_next = [](triple v) {
      printf("OnNext: %d, %d, %d\n", get<0>(v), get<1>(v), get<2>(v));
  };

  values.take(5).subscribe(on_next);
}
