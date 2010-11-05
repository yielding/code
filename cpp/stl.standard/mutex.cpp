#include <rw/stdmutex.h>

#include <iostream>

using namespace std;

#ifdef _RWSTD_MULTI_THREAD
//
// An integer shared amongst multiple threads.
//
int I;

//
// A mutex used to synchronize updates to I.
//
_RWSTDMutex I_mutex;

//
// Increment I by one.  Uses a _RWSTDMutex directly.
//

void increment_I ()
{
   I_mutex.acquire();  // Lock the mutex.
   I++;
   I_mutex.release();  // Unlock the mutex.
}

//
// Decrement I by one.  Uses a _RWSTDGuard.
//

void decrement_I ()
{
   _RWSTDGuard guard(I_mutex);  // Acquire the lock on I_mutex.
   --I;
   //
   // The lock on I is released when destructor is called on guard.
   //
}
#endif


int main()
{
#ifdef _RWSTD_MULTI_THREAD
  increment_I();
  decrement_I();
  cout << "Multi Threading enabled" << endl;
#else
  cout << "Multi Threading not enabled" << endl;
#endif
  return 0;
}


