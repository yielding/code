#include "stlexam.h"

#include <string>
#include <stdexcept>
#include <iostream>

using namespace std;

// A simple class to demonstrate throwing an exception
static void f() { throw runtime_error("a runtime error"); }

int main ()
{
    string s;

    // First we'll try to incite and catch an exception from
    // the standard library string class.
    // We'll try to replace at a position that is non-existant.
    //
    // By wrapping the body of main in a try-catch block we can be
    // assured that we'll catch all exceptions in the exception hierarchy.
    // You can simply catch exception as is done below, or you can catch
    // each of the exceptions in which you have an interest.
    try
    {
        s.replace(100,1,1,'c');
    }
    catch (const exception& e)
    {
        cout << "Got an exception: " << e.what() << endl;
    }

    // Now we'll throw our own exception using the function 
    // defined above.
    try
    {
        f();
    }
    catch (const exception& e)
    {
        cout << "Got an exception: " << e.what() << endl;
    }

    return 0;
}
