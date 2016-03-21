#include <iostream>
#include <string>
#include <vector>
#include <utility>
#include <cstdio>

#include "rxcpp/rx.hpp"

using namespace std;

int main(int argc, char *argv[])
{
  auto count = 100;

  auto values = rxcpp::observable<>::range(1);

  auto s1 = values
    .map([](int prime) { return make_tuple("1:", prime); });
           
  auto s2 = values
    .map([](int prime) { return make_tuple("2:", prime); });
  
  s1.merge(s2)
    .take(6)
    .as_blocking()
    .subscribe(rxcpp::util::apply_to(
      [](const char* s, int p) {
        printf("%s %d\n", s, p);
      }));

  return 0;
}
