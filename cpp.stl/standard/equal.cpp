#include "stlexam.h"
#include <algorithm>
#include <vector>
#include <functional>
#include <iostream>

using namespace std;

int 
main()
{
   int d1[4] = {1,2,3,4};
   int d2[4] = {1,2,4,3};
   //
   // Set up two vectors.
   //
   vector<int> v1(d1+0, d1+4), v2(d2+0, d2+4);
   //
   // Check for equality.
   //
   bool b1 = equal(v1.begin(), v1.end(), v2.begin());
   bool b2 = equal(v1.begin(), v1.end(), v2.begin(),equal_to<int>());
   //
   // Both b1 and b2 are false.
   //
   cout << (b1 ? "TRUE" : "FALSE")  << " " 
        << (b2 ? "TRUE" : "FALSE") << endl;
   return 0;
}
