#include <iostream>
#include <string>
#include <vector>
#include <chrono>

#include "rxcpp/rx.hpp"

using namespace std;
      namespace rx=rxcpp;

int main(int argc, char *argv[])
{
  auto base = rx::observable<>::interval(chrono::milliseconds(30))
    .take(3)
    .map([](long) {
        return rx::observable<>::interval(chrono::microseconds(10)).as_dynamic();
     });

  auto values = base.switch_on_next().take(10);
  values.subscribe(
      [](long v) {printf("OnNext: %ld\n", v);},
      []() {printf("OnCompleted\n");
  });
  
  return 0;
}
