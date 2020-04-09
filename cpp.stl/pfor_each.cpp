#include <iostream>
#include <vector>
#include <algorithm>
#include <execution>

using namespace std;

int main(int argc, char *argv[])
{
  int a[] = {0, 1};
  vector<int> v {3, 4};

  for_each(execution::par, begin(a), end(a), [&](int i) {
    v.push_back(i*2 + 1);
  });

  for (auto _ : v)
    cout << "hi" << endl;


  return 0;
}
