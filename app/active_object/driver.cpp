#include "active_object.h"

#include <iostream>

void print()
{
  std::cout << "eee\n";
}

int main(int argc, char const* argv[])
{
  sys::active_object o;

  o.post(print);
  o.scheduler().run();
  
  return 0;
}
