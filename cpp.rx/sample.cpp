#include <iostream>
#include <string>
#include <vector>

#include "rxcpp/rx.hpp"

using namespace std;

int main(int argc, char *argv[])
{
  auto ints = rxcpp::observable<>::create<int>(
    [](rxcpp::subscriber<int> s) {
      s.on_next(1);
      s.on_next(2);
      s.on_completed();
  });

  ints.subscribe(
    [](int v) { cout << "on_next: " << v << endl; },
    []        { cout << "completed" << endl; }
  );

  return 0;
}
