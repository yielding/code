#define CATCH_CONFIG_MAIN

#include "catch.hpp"
#include "rxcpp/rx.hpp"

#include "get_pid.h"

using namespace std;
      namespace rx = rxcpp;

SCENARIO("buffer count sample") 
{
  auto on_next = [](vector<int> vs) {
    cout << "OnNext";
    for (auto v: vs) cout << " " << v;
    cout << endl;
  };

  auto val = rx::observable<>::range(1, 10).buffer(3);
  val.subscribe(on_next);
}

SCENARIO("buffer count+skip sample") 
{
  auto on_next = [](vector<int> vs) {
    cout << "OnNext";
    for (auto v: vs) cout << " " << v;
    cout << endl;
  };

  auto val = rx::observable<>::range(1, 7).buffer(2, 3);
  val.subscribe(on_next);
}

SCENARIO("buffer count+skip+coordination sample") 
{
  auto period = chrono::milliseconds(4);
  auto skip   = chrono::milliseconds(6);
  auto values = rx::observable<>::interval(chrono::steady_clock::now() + chrono::milliseconds(1), chrono::milliseconds(2))
                  .map([](long v) {
                    printf("[thread %s] Interval OnNext: %ld\n", get_pid().c_str(), v);
                    return v;
                   })
                  .take(7)
                  .buffer_with_time(period, skip, rx::observe_on_new_thread());

  auto on_next = [](vector<long> vs) {
    printf("[thread %s] OnNext:", get_pid().c_str());
    for (auto v: vs) printf(" %ld", v);
    printf("\n");
  };

  auto on_complete = [] {
    printf("[thread %s] OnCompleted\n", get_pid().c_str());
  };

  values.as_blocking().subscribe(on_next);
}
