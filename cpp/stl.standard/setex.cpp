#include <set>
 
#include <iostream>

using namespace std;

typedef set<double> set_type;

ostream& operator<<(ostream& out, const set_type& s)
{
  copy(s.begin(), s.end(), ostream_iterator<set_type::value_type>(cout," "));
  return out;
}

int main ()
{
  //
  // Create a set of double's, and one of integers.
  //
  set_type   sd;
  int         i;

  for (i = 0; i < 10; ++i)
    //
    // Insert values.
    //
    sd.insert(i);
  //
  // Print out the set.
  //
  cout << sd << endl << endl;
  //
  // Now let's erase half of the elements in the set.
  //
  int half = sd.size() / 2;
  set_type::iterator sdi = sd.begin();
  advance(sdi,half);
  sd.erase(sd.begin(),sdi);
  //
  // Print it out again.
  //
  cout << sd << endl << endl;
  //
  // Make another set and an empty result set.
  //
  set_type sd2, sdResult;
  for (i = 1; i < 9; i++)
    sd2.insert(i+5);
  cout << sd2 << endl;
  //
  // Try a couple of set algorithms.
  //
  set_union(sd.begin(),sd.end(),sd2.begin(),sd2.end(),
            inserter(sdResult,sdResult.begin()));
  cout << "Union:" << endl << sdResult << endl;

  sdResult.erase(sdResult.begin(),sdResult.end());
  set_intersection(sd.begin(),sd.end(), sd2.begin(),sd2.end(),
                   inserter(sdResult,sdResult.begin()));
  cout << "Intersection:" << endl << sdResult << endl;
  
  return 0;
}
