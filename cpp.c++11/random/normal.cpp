#include <iostream>
#include <iomanip>
#include <string>
#include <map>
#include <random>
#include <cmath>

using namespace std;

int main()
{
  random_device rd;
  mt19937 gen(rd());

  normal_distribution<> d(5, 2);

  map<int, int> hist;
  for (int n=0; n<10000; ++n)
    ++hist[round(d(gen))];

  for (auto p : hist)
      cout << fixed   << setprecision(1) << setw(2)
           << p.first << ' ' << string(p.second/200, '*') << '\n';
}
