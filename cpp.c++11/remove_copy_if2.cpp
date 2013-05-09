#include <iostream>
#include <algorithm>
#include <vector>

struct not_a_catalog_record {
  template <typename T>
  bool operator()(T& k) {
    return k < 5;
  }
};

int main(int argc, char const* argv[])
{
  std::vector<int> arr = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };
  std::vector<int> res;

  remove_copy_if(arr.begin(), arr.end(), back_inserter(res), 
    [](int i) { return i % 2 == 0; });

  for (auto &i : res) std::cout << i << " ";

  return 0;
}
