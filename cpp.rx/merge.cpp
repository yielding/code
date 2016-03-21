#include <iostream>
#include <string>
#include <vector>
#include <cctype>

#include "rxcpp/rx.hpp"

int main(int argc, char *argv[])
{
  auto keys 
    = rxcpp::observable<>::create<int> (
        [](rxcpp::subscriber<int> dest) {
          for (;;) {
            int key = std::cin.get();
            dest.on_next(key);
          }
        }).publish();
  
  auto a = keys.filter([](int k) { return std::tolower(k) == 'a';});
  auto g = keys.filter([](int k) { return std::tolower(k) == 'g';});

  a.merge(g).
    subscribe([](int k) {
      std::cout << k << std::endl;
    });

  keys.connect();

  return 0;
}
