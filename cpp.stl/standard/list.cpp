#include "stlexam.h"
#include <list>
#include <string>

#include <iostream>

using namespace std;

//
// Print out a list of strings.
//
ostream& 
operator<< (ostream& out, const list<string>& l)
{
    copy(l.begin(), l.end(), ostream_iterator<string>(out," "));
    return out;
}

int main()
{
    //
    // Create a list of critters.
    //
    list<string> critters;
    int i;
    //
    // Insert several critters.
    //
    critters.insert(critters.begin(),"antelope");
    critters.insert(critters.begin(),"bear");
    critters.insert(critters.begin(),"cat");
    //
    // Print out the list.
    //
    cout << critters << endl;
    // 
    // Change cat to cougar.
    //
    *find(critters.begin(),critters.end(),"cat") = "cougar";
    cout << critters << endl;
    //
    // Put a zebra at the beginning, an ocelot ahead of antelope,
    // and a rat at the end.
    //
    critters.push_front("zebra");
    critters.insert(find(critters.begin(),critters.end(),"antelope"),"ocelot");
    critters.push_back("rat");
    cout << critters << endl;
    //
    // Sort the list (Use list's sort function since the 
    // generic algorithm requires a random access iterator 
    // and list only provides bidirectional)
    //
    critters.sort();
    cout << critters << endl;
    //
    // Now let's erase half of the critters.
    //
    int half = critters.size() / 2;
    for (i=0; i<half; ++i) critters.erase(critters.begin());
    cout << critters << endl;

    return 0;
}
