#include <iostream>
#include <future>

using namespace std;

void hello() { cout << "world"; }

int main(int argc, const char *argv[])
{
  async(launch::async, []{ hello(); });

  return 0;
}
