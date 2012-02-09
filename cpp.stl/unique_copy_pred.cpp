#include <algorithm>
#include <boost/format.hpp>
#include <iostream>
#include <set>
#include <vector>

using namespace std;
using namespace boost;

template <typename C>
void pp(C& c)
{
  for (auto it=c.begin(); it!=c.end(); ++it) cout << *it << " ";
  cout << endl;
}

struct record
{
  record(int n, string s) :no(n), name(s) 
  {}

  bool operator< (record const& rhs) const
  {
    return no < rhs.no;
  }

  bool operator==(record const& rhs) const
  {
    return no == rhs.no;
  }

  int no;
  string name;
};

int main(int argc, char const* argv[])
{
  vector<record> arr;
  vector<record> res;

  record r1(1, "leech"), 
         r2(2, "kamin"),
         r3(1, "gunhee"),
         r4(3, "gunree"),
         r5(3, "gnree");
  record r6(1, "lch"); 

  arr.push_back(r1);
  arr.push_back(r2);
  arr.push_back(r3);
  arr.push_back(r4);
  arr.push_back(r5);
  arr.push_back(r6);

  auto comp = [](record const& r1, record const& r2)
  {
    return r1.no == r2.no;
  };

  sort(arr.begin(), arr.end());
  unique_copy(arr.begin(), arr.end(), back_inserter(res));

  for (auto it=res.begin(); it!=res.end(); ++it)
    cout << it->no << " ";

  return 0;
}
