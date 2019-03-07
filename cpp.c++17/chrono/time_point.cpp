#include <chrono>
#include <ctime>
#include <iostream>
#include <iomanip>
#include "date.h"

using namespace std;
using namespace date;

int main(int argc, char const* argv[])
{
  // 1. time_point
  auto now = chrono::system_clock::now();
  cout << now;

  return 0;
}
