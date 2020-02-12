#include <iostream>
#include <string>
#include <vector>
#include <algorithm>

using namespace std;

typedef pair<int, string> data;

int main(int argc, char* argv[])
{
  vector<data> v, v1;

  v.push_back(make_pair(1, "string1"));
  v.push_back(make_pair(1, "string1"));
  v.push_back(make_pair(3, "string3"));
  v.push_back(make_pair(2, "string2"));
  v.push_back(make_pair(2, "string2"));
  v.push_back(make_pair(4, "string4"));
  v.push_back(make_pair(2, "string2"));

  auto c0 = [](data const& d1, data const& d2) { return d1.first  < d2.first; };
  auto c1 = [](data const& d1, data const& d2) { return d1.first == d2.first; };
  sort(v.begin(), v.end(), c0);

  for (auto i=v.begin(); i!=v.end(); ++i)
    cout  << i->first << " " << i->second << endl;

  unique_copy(v.begin(), v.end(), back_inserter(v1), c1);
  for (auto i=v1.begin(); i!=v1.end(); ++i)
    cout  << i->first << " " << i->second << endl;

  return 0;
}
