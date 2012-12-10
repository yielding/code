#include <iostream>
#include <vector>
#include <cstdlib>
#include <ctime>
#include <sstream>
#include <algorithm>

using namespace std;

int sum(vector<int> const& v)
{
  int s = 0;
  for (auto it=v.begin(); it!=v.end(); ++it)
    s += *it;

  return s;
}

template <typename T>
auto join(vector<T>& v) -> string
{
  auto it = v.begin();
  stringstream ss; ss << *it;

  for (++it; it != v.end(); ++it)
    ss << ", " << *it;

  return ss.str();
}

void sort(vector<int>& v)
{
  sort(v.begin(), v.end(), [](int a, int b) { return a < b; });
}

int main(int argc, const char *argv[])
{
  vector<int> v;

  srand(time(nullptr));
  for (int i=0; i<10; i++) 
  {
      v.push_back(rand()  % 100);
      sort(v);
      cout << "arr: [" << join(v) << "]\n";

      auto s = sum(v);
      auto m = float(s) / v.size();
      cout << "sum: " << s << endl;
      printf("avr: %5.2f\n", m);
      cout << "avr: " << m << endl;
  }

  return 0;
}
