#include "sys/active_object.hpp"

#include <iostream>

using namespace std;
using namespace chrono_literals;

int main(int argc, char* argv[])
{
  sys::active_object o;

  this_thread::sleep_for(1s);

  cout << "now start...\n";

  o.post([] { cout << "hello world first " << endl; });
  o.dispatch([] { cout << "hello world second" << endl; });

  this_thread::sleep_for(1s);

  return 0;
}