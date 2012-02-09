#include <iterator>
#include <vector>
#include <iostream>

using namespace std;

int main ()
{
  int arr[4] = {3,4,7,8};
  vector<int> v(arr+0, arr+4);

  cout << "Traversing vector with iterator: " << endl << "     ";
  for (vector<int>::iterator i = v.begin(); i != v.end(); i++)
    cout << *i << " ";

  vector<int>::reverse_iterator rev(v.end());
  vector<int>::reverse_iterator rev_end(v.begin());

  cout << endl << endl;
  cout << "Same vector, same loop, reverse_itertor: " << endl << "     ";
  for(; rev != rev_end; rev++) cout << *rev << " ";

  cout << endl;
  return 0;
}
