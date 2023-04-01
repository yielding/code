#include <iostream>
#include <vector>
#include <range/v3/all.hpp>

using namespace std;

int main() 
{
  // Create a vector of numbers from 1 to 5
  vector<int> vec = {1, 2, 3, 4, 5};

  // Create a cycle view of the vector
  auto cycle_view = vec | ::ranges::views::cycle;

  // Print the first 10 elements of the cycle view
  for (int i = 0; i < 10; i++)
    cout << cycle_view[i] << ' ';

  cout << endl;

  return 0;
}