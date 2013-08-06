#include <iostream>
#include <algorithm>
#include <vector>

using namespace std;

int main(int argc, char const* argv[])
{
  vector<int> arr { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };
  vector<int> res;

  remove_copy_if(arr.begin(), arr.end(), 
                 back_inserter(res), 
                 [](int i) { return i % 2 == 0; });

  cout << "size: " << res.size() << endl;
  for (auto i : res) cout << i << " ";
  cout << endl;

  return 0;
}
