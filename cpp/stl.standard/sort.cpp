#include <vector>
#include <algorithm>
#include <functional>
 
#include <iostream>

using namespace std;

struct associate
{
  int num;
  char chr;
  associate(int n, char c) : num(n), chr(c){};
  associate() : num(0), chr('\0'){};
};

bool operator< (const associate &x, const associate &y)
{
  return x.num < y.num;
}

ostream& operator<< (ostream &s, const associate &x)
{
  return s << "<" << x.num << ";" << x.chr << ">";
}

int main ()
{
  vector<associate,allocator<associate> >::iterator i, j, k;

  associate arr[20] = 
  {associate(-4, ' '), associate(16, ' '),
   associate(17, ' '), associate(-3, 's'),
   associate(14, ' '), associate(-6, ' '),
   associate(-1, ' '), associate(-3, 't'),
   associate(23, ' '), associate(-3, 'a'),
   associate(-2, ' '), associate(-7, ' '),
   associate(-3, 'b'), associate(-8, ' '),
   associate(11, ' '), associate(-3, 'l'),
   associate(15, ' '), associate(-5, ' '),
   associate(-3, 'e'), associate(15, ' ')};
  //
  // Set up vectors.
  //
  vector<associate> v(arr+0, arr+20), v1((size_t)20), v2((size_t)20);
  //
  // Copy original vector to vectors #1 and #2.
  //
  copy(v.begin(), v.end(), v1.begin());
  copy(v.begin(), v.end(), v2.begin());
  //
  // Sort vector #1.
  //
  sort(v1.begin(), v1.end());
  //
  // Stable sort vector #2.
  //
  stable_sort(v2.begin(), v2.end());
  //
  // Display the results.
  //
  cout << "Original    sort      stable_sort" << endl;
  for(i = v.begin(), j = v1.begin(), k = v2.begin();
      i != v.end(); i++, j++, k++)
    cout << *i << "     " << *j << "     " << *k << endl;

  return 0;
}
