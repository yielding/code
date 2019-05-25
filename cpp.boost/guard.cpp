#include <iostream>
#include <memory>
#include <functional>
#include <boost/shared_ptr.hpp>

using namespace std;

void deleter(uint8_t* p)
{
  cout << "deleting...\n";
  cout << addressof(p) << endl;
  delete [] p;
}

int main(int argc, char const* argv[])
{
  auto x = new uint8_t[10];

  boost::shared_ptr<uint8_t> ptr(x, deleter);
  cout << "exiting ...\n";
  cout << addressof(x) << endl;

  return 0;
}
