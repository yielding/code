#include <iterator>
#include <list>
#include <iostream>

int main ()
{
  using namespace std;
  //
  // Initialize a list using an array.
  //
  int arr[6] = {3,4,5,6,7,8};
  list<int,allocator<int> > l(arr+0, arr+6);
  //
  // Declare a list iterator, s.b. a ForwardIterator.
  //
  list<int>::iterator itr = l.begin();
  //
  // Output the original list.
  //
  cout << "For the list: ";
  copy(l.begin(),l.end(),ostream_iterator<int,char,char_traits<char> >(cout," "));
  cout << endl << endl;
  cout << "When the iterator is initialized to l.begin()," 
    << endl << "it points to " << *itr << endl << endl;
  //
  // operator+ is not available for a ForwardIterator, so use advance.
  //
  advance(itr, 4);
  cout << "After advance(itr,4), the iterator points to " << *itr << endl;

  return 0;
}
