#include <algorithm>
#include <iostream>
#include <vector>

using namespace std;

// API: Application Programming Interface
//      C/C++ library
//      system call

int main(int argc, const char *argv[])
{
  vector<int> data = { 1, 2, 3, 4, 5, 7, 8, 9, 10, 11, 12 };

  do
  {
    for (auto it=data.begin(); it!=data.end(); ++it)
      cout << *it << " ";

    cout << endl;
  }
  while(next_permutation(data.begin(), data.end()));

  return 0;
}
