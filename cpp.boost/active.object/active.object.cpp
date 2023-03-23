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
  cout << "in between 1 ...\n";
  this_thread::sleep_for(1ms);
  cout << "in between 2 ...\n";
  cout << "in between 3 ...\n";
  o.post([] { cout << "hello world second" << endl; });
  //o.dispatch([] { cout << "hello world second" << endl; });

  this_thread::sleep_for(1s);

  return 0;
}