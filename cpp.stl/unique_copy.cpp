#include <iostream>
#include <string>
#include <vector>
#include <algorithm>
#include <utility>

using namespace std;

using user_data = pair<int, string>;

int main(int argc, char* argv[])
{
  vector<user_data> v, v1;

  v.push_back(make_pair(1, "string1"));
  v.push_back(make_pair(1, "string1"));
  v.push_back(make_pair(3, "string3"));
  v.push_back(make_pair(2, "string2"));
  v.push_back(make_pair(2, "string2"));
  v.push_back(make_pair(4, "string4"));
  v.push_back(make_pair(2, "string2"));

  auto c0 = [](user_data const& d1, user_data const& d2) { return d1.first < d2.first; };
  auto c1 = [](user_data const& d1, user_data const& d2) { return d1.first == d2.first; };
  sort(v.begin(), v.end(), c0);

  for (auto it: v)
    cout << it.first << " " << it.second << endl;

  unique_copy(v.begin(), v.end(), back_inserter(v1), c1);
  for (auto it: v1)
    cout << it.first << " " << it.second << endl;

  return 0;
}