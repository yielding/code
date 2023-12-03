#include <map>
#include <string>
#include <iostream>

using namespace std;

pair<int, bool> return_pair()
{
  return {1, true};
}

int main(int argc, char *argv[])
{
  if (auto[it, succeeded] = return_pair(); succeeded)
  {
    cout << "ok value = " << it << endl;
  }
  
  return 0;
}
