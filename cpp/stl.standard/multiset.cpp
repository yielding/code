#include <set>

#include <iostream>

using namespace std;

typedef multiset<int> set_type;

ostream& operator<< (ostream& out, const set_type& s)
{
  copy(s.begin(),s.end(),ostream_iterator<set_type::value_type,char,char_traits<char> >(cout," "));
  return out;
}

int main ()
{
  //
  // Create a multiset of integers.
  //
  set_type  si;
  int       i;

  for (int j = 0; j < 2; j++)
  {
    for (i = 0; i < 10; ++i)
      //
      // Insert values with a hint.
      //
      si.insert(si.begin(), i);
  }
  //
  // Print out the multiset.
  //
  cout << si << endl;
  //
  // Make another int multiset and an empty multiset.
  //
  set_type si2, siResult;
  for (i = 0; i < 10; i++)
    si2.insert(i+5);
  cout << si2 << endl;
  //
  // Try a couple of set algorithms.
  //
  set_union(si.begin(),si.end(),si2.begin(),si2.end(),
      inserter(siResult,siResult.begin()));
  cout << "Union:" << endl << siResult << endl;

  siResult.erase(siResult.begin(),siResult.end());
  set_intersection(si.begin(),si.end(),si2.begin(),si2.end(),
      inserter(siResult,siResult.begin()));
  cout << "Intersection:" << endl << siResult << endl;

  return 0;
}
