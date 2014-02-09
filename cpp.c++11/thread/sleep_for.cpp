#include <iostream>
#include <chrono>
#include <thread>

using namespace std;

int main(int argc, const char *argv[])
{
  cout << "Hello waiter \n";
  chrono::milliseconds duration(2000);
  this_thread::sleep_for(duration);
  cout << "waited 2000 ms\n";
  
  return 0;
}
