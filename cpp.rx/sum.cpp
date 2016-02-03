#include <iostream>
#include <string>
#include <vector>

#include "rxcpp/rx.hpp"

using namespace std;
      namespace rx = rxcpp;

int main(int argc, char *argv[])
{
  auto v = rx::observable<>::range(1, 10).sum();

  v.subscribe(
      [](int v) { cout << v << endl; },
      []{}
  );

  return 0;
}
