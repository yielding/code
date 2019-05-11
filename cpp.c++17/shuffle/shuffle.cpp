#include <random>
#include <algorithm>
#include <iterator>
#include <iostream>

using namespace std;

int main(int argc, char *argv[])
{
  vector<int> v = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };

  random_device rd;
  mt19937 g(rd());

  shuffle(v.begin(), v.end(), g);
  copy(v.begin(), v.end(), ostream_iterator<int>(cout, " "));
  cout << endl;
  
  return 0;
}
