#include <iostream>
#include "probes.h"

using namespace std;

int const SIZE = 100000;
long primes[SIZE] = { 3 };
long primecount = 1;

int main(int argc, char const* argv[])
{
  long divisor = 0;
  long currentprime = 5;
  long isprime = 1;

  if (PRIMES_PRIMECALC_START_ENABLED())
    cout << "1\n";
  else
    cout << "0\n";

  while (currentprime < SIZE)
  {
    isprime = 1;
    PRIMES_PRIMECALC_START(currentprime);
    for (divisor=0; divisor<primecount; divisor++)
    {
      if (currentprime % primes[divisor] == 0)
        isprime = 0;
    }

    PRIMES_PRIMECALC_DONE(currentprime, isprime);
    if (isprime)
    {
      primes[primecount++] = currentprime;
      PRIMES_PRIMECALC_TABLESIZE(primecount);
      printf("%ld is a prime\n", currentprime);
    }

    currentprime = currentprime + 2;
  }

  return 0;
}
