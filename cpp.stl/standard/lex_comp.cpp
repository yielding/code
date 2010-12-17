#include "stlexam.h"
#include <algorithm>
#include <vector>
#include <functional>

#include <iostream>

using namespace std;

int 
main()
{							 
    int d1[5] = {1,3,5,32,64}; 		
    int d2[5] = {1,3,2,43,56};
    // 
    // Set up vector.
    //
    vector<int> v1(d1+0, d1+5), v2(d2+0, d2+5);	 
    // 
    // Is v1 less than v2 (I think not).
    //
    bool b1 = lexicographical_compare(v1.begin(),v1.end(),v2.begin(),v2.end());
    //
    // Is v2 less than v1 (yup, sure is).
    //
    bool b2 = lexicographical_compare(v2.begin(), v2.end(),
            v1.begin(), v1.end(), less<int>());
    cout << (b1 ? "TRUE" : "FALSE") << " " 
        << (b2 ? "TRUE" : "FALSE") << endl;

    return 0;
}
