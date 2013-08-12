#include <iostream>
#include <algorithm>
#include <utility>

using namespace std;

int main(int argc, const char *argv[])
{
  int arr[10] = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
  int tarr[5] = { 0 };
  int farr[5] = { 0 };

  partition_copy(begin(arr), end(arr), 
                 begin(tarr), 
                 begin(farr),
                 [](int i) { return i > 5; });

  cout << "true arr: ";
  for (auto it: tarr) cout << it << ' ';
  cout << endl;

  cout << "false arr: ";
  for (auto it: farr) cout << it << ' ';
  cout << endl;

  return 0;
}
