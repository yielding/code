#include "stlexam.h"
#include <compnent.h>

#include <iostream>
#include <stdexcept>

using namespace std;

static void f() { throw runtime_error("a runtime error"); }

int main ()
{
   //
   // By wrapping the body of main in a try-catch block we can be
   // assured that we'll catch all exceptions in the exception hierarchy.
   // You can simply catch exception as is done below, or you can catch
   // each of the exceptions in which you have an interest.
   //
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
