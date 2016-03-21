#define CATCH_CONFIG_MAIN

#include "catch.hpp"
#include "rxcpp/rx.hpp"

#include "get_pid.h"

using namespace std;
      namespace rx = rxcpp;

using triple = tuple<int, int, int>;
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
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

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
SCENARIO("coordination combine_latest sample") {
  auto thr = rx::synchronize_event_loop();
  auto o1  = rx::observable<>::interval(chrono::milliseconds(2)).map([](int v) {
    printf("[thread %s] Source1 OnNext: %d\n", get_pid().c_str(), v);
    return v;
  });

  auto o2  = rx::observable<>::interval(chrono::milliseconds(3)).map([](int v) {
    printf("[thread %s] Source2 OnNext: %d\n", get_pid().c_str(), v);
    return v;
  });

  auto o3  = rx::observable<>::interval(chrono::milliseconds(5)).map([](int v) {
    printf("[thread %s] Source3 OnNext: %d\n", get_pid().c_str(), v);
    return v;
  });

  auto values  = o1.combine_latest(thr, o2, o3);
  auto on_next = [](triple v) {
    printf("[thread %s] OnNext: %d, %d, %d\n", 
            get_pid().c_str(), get<0>(v), get<1>(v), get<2>(v));
  };

  values.take(5).as_blocking().subscribe(on_next);
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
SCENARIO("Selector combine_latest sample") {
  printf("//! [Selector combine_latest sample]\n");

  auto o1 = rx::observable<>::interval(chrono::milliseconds(2));
  auto o2 = rx::observable<>::interval(chrono::milliseconds(3));
  auto o3 = rx::observable<>::interval(chrono::milliseconds(5));
  auto values = o1.combine_latest(
      [](int v1, int v2, int v3) { return 100 * v1 + 10 * v2 + v3; },
      o2, 
      o3);

  values
    .take(5)
    .subscribe([](int v) { printf("OnNext: %d\n", v); },
               []        { printf("OnCompleted\n");   });

  printf("//! [Selector combine_latest sample]\n");
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
