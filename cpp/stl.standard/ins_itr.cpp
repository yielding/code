// refer (1) 2009.04.20
#include <iterator>
#include <deque>

#include <iostream>


int main ()
{
  using namespace std;

  ostream_iterator<int> out(cout," ");

  //
  // Initialize a deque using an array.
  //
  int arr[4] = { 3,4,7,8 };
  deque<int> d(arr+0, arr+4);
  //
  // Output the original deque.
  //
  cout << "Start with a deque: " << endl << "     ";
  copy(d.begin(), d.end(), out);
  //
  // Insert into the middle.
  //
  insert_iterator< deque<int> > ins(d, d.begin()+2);
  *ins = 5; *ins = 6;
  //
  // Output the new deque.
  //
  cout << endl << endl;
  cout << "Use an insert_iterator: " << endl << "     ";
  copy(d.begin(), d.end(), out);
  //
  // A deque of four 1s.
  //
  deque<int> d2(4, 1);
  //
  // Insert d2 at front of d.
  //
  copy(d2.begin(), d2.end(), front_inserter(d));
  //
  // Output the new deque.
  //
  cout << endl << endl;
  cout << "Use a front_inserter: " << endl << "     ";
  copy(d.begin(), d.end(), out);
  //
  // Insert d2 at back of d.
  //
  copy(d2.begin(), d2.end(), back_inserter(d));
  //
  // Output the new deque.
  //
  cout << endl << endl;
  cout << "Use a back_inserter: " << endl << "     ";
  copy(d.begin(), d.end(), out);

  cout << endl;

  return 0;
}
