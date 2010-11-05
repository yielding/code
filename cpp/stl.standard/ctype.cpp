#include "stlexam.h"
#pragma hdrstop

#include <iostream>

using namespace std;

int main ()
{
    locale loc;
    string s1("blues Power");

    // Get a reference to the ctype<char> facet
    const ctype<char>& ct = use_facet<ctype<char> >(loc);

    // Check the classification of the 'a' character
    cout << ct.is(ctype_base::alpha,'a') << endl;
    cout << ct.is(ctype_base::punct,'a') << endl;

    // Scan for the first upper case character
    cout << (char)*(ct.scan_is(ctype_base::upper, s1.begin(),s1.end())) << endl;

    // Convert characters to upper case
    ct.toupper(s1.begin(),s1.end());
    cout << s1 << endl;

    return 0;
}
