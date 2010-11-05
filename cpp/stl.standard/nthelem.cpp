#include<algorithm>
#include<vector>

#include <iostream>

template<class RandomAccessIterator>
void quik_sort(RandomAccessIterator start, RandomAccessIterator end)
{
  using std::distance;
  using std::nth_element;
  using std::swap;

  size_t dist = 0;
  distance(start, end, dist);
  //
  // Stop condition for recursion.
  //
  if(dist > 2)
  {
    //
    // Use nth_element to do all the work for quik_sort.
    //
    nth_element(start, start+(dist/2), end);
    //
    // Recursive calls to each remaining unsorted portion.
    //
    quik_sort(start, start+(dist/2-1));
    quik_sort(start+(dist/2+1), end);
  }

  if(dist == 2 && *end < *start)
    swap(start, end);
}

int main ()
{
  using namespace std;

  //
  // Initialize a vector using an array of integers.
  //
  int arr[10] = {37, 12, 2, -5, 14, 1, 0, -1, 14, 32};
  vector<int> v(arr+0, arr+10);
  //
  // Print the initial vector.
  //
  cout << "The unsorted values are: " << endl << "     ";
  vector<int>::iterator i; 
  for(i = v.begin(); i != v.end(); i++)
    cout << *i << ", ";
  cout << endl << endl;
  //
  // Use the new sort algorithm.
  //
  quik_sort(v.begin(), v.end());
  //
  // Output the sorted vector.
  //
  cout << "The sorted values are: " << endl << "     ";
  for(i = v.begin(); i != v.end(); i++)
    cout << *i << ", ";
  cout << endl << endl;

  return 0;
}
