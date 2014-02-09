#include <iostream>
#include <future>
#include <vector>

using namespace std;

int main(int argc, const char *argv[])
{
  vector<future<int>> futures;

  for (int i=0; i<10; ++i) 
    futures.push_back(async([](int m) {return 2*m;}, i));

  for (auto& e: futures) 
    cout << e.get() << endl;

  return 0;
}
