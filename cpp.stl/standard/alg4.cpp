#include <list>
#include <set>
#include <algorithm>

#include <iostream>

using namespace std;

bool isEven (int n) { return 0 == (n % 2); }

//
// Illustrate the use of the remove algorithm.
//

void remove_example ()
{
  cout << "Remove Algorithm examples" << endl;
  //
  // Create a list of numbers.
  //
  int data[] = { 1, 2, 4, 3, 1, 4, 2 };
  list<int> aList;
  copy (data, data+7, inserter(aList, aList.begin()));
  cout << "Original list: ";
  copy (aList.begin(), aList.end(), ostream_iterator<int>(cout, " "));
  cout << endl;
  //
  // Remove 2's, copy into a new list.
  //
  list<int> newList;
  remove_copy (aList.begin(), aList.end(), back_inserter(newList), 2);
  cout << "After removing 2's: ";
  copy (newList.begin(), newList.end(), ostream_iterator<int>(cout, " "));
  cout << endl;
  //
  // Remove 2's in place.
  //
  list<int>::iterator where;
  where = remove(aList.begin(), aList.end(), 2);
  cout << "List after removal, before erase: ";
  copy (aList.begin(), aList.end(), ostream_iterator<int>(cout, " "));
  cout << endl;
  aList.erase(where, aList.end());
  cout << "List after erase: ";
  copy (aList.begin(), aList.end(), ostream_iterator<int>(cout, " "));
  cout << endl;
  //
  // Remove all even values.
  //
  where = remove_if (aList.begin(), aList.end(), isEven);
  aList.erase(where, aList.end());
  cout << "List after removing even values: ";
  copy (aList.begin(), aList.end(), ostream_iterator<int>(cout, " "));
  cout << endl;
}

//
// Illustrate use of the unqiue algorithm.
//
void unique_example ()
{
  //
  // First make a list of values.
  //
  int data[] = { 1, 3, 3, 2, 2, 4 };
  list<int> aList;
  copy(data, data+6, inserter(aList, aList.begin()));
  cout << "Origianal List: ";
  copy(aList.begin(), aList.end(), ostream_iterator<int>(cout, " "));
  cout << endl;
  //
  // Copy unique elements into a set.
  //
  set<int> aSet;
  //    unique_copy(aList.begin(), aList.end(), inserter(aSet, aSet.begin()));
  cout << "Set after unique_copy: ";
  copy(aSet.begin(), aSet.end(), ostream_iterator<int>(cout, " "));
  cout << endl;
  //
  // Copy unique elements in place.
  //
  list<int>::iterator where;
  where = unique(aList.begin(), aList.end());
  cout << "List after calling unique: ";
  copy(aList.begin(), aList.end(), ostream_iterator<int>(cout, " "));
  cout << endl;
  //
  // Remove trailing values.
  //
  aList.erase(where, aList.end());
  cout << "List after erase: ";
  copy(aList.begin(), aList.end(), ostream_iterator<int>(cout, " "));
  cout << endl;
}

int main ()
{
  cout << "STL generic algorithms -- Removal Algorithms" << endl;

  remove_example();
  unique_example();

  cout << "End of removal algorithms sample program" << endl;

  return 0;
}
