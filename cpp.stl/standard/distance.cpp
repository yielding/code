#include "stlexam.h"
#include<iterator>
#include<vector>

#include <iostream>

using namespace std;

int 
main()
{
   //
   // Initialize a vector using an array.
   //
   int arr[6] = {3,4,5,6,7,8};
   vector<int> v(arr+0, arr+6);
   //
   // Declare a list iterator, s.b. a ForwardIterator.
   //
   vector<int>::iterator itr = v.begin()+3;
   //
   // Output the original vector.
   //
   cout << "For the vector: ";
   copy(v.begin(), v.end(), ostream_iterator<int>(cout," "));
   cout << endl << endl;

   cout << "When the iterator is initialized to point to " << *itr << endl;
   //
   // Use of distance.
   //
   vector<int>::difference_type dist = 0;
   distance(v.begin(), itr, dist);
   cout << "The distance between the beginning and itr is " << dist << endl;

   return 0;
}
