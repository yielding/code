using System;
using System.Collections.Generic;
using System.Collections;

namespace euler1 {
  class Problem10 {

    const int numm = 2000000;

    public static void Main(string[] args) {
      new Problem10().TrialDivision();
      new Problem10().SieveOfEratosthenes();
      new Problem10().SieveOfAtkin();
    }

#region Trial Division
    public void TrialDivision() {
      DateTime startTime = DateTime.Now;

      int[] primes = TrialDivisionPrimes(numm);
      decimal primeSum = 0;

      for (int i=0; i<primes.Length; i++)
        primeSum += primes[i];

      DateTime stopTime = DateTime.Now;
      TimeSpan duration = stopTime - startTime;
      Console.WriteLine("Prime sum of all primes below {0} is {1} ", numm, primeSum);
      Console.WriteLine("Solution took {0} ms using Trial Division", duration.TotalMilliseconds);
    }

    private int[] TrialDivisionPrimes(int upperLimit) {
      int counter = 3;
      bool isPrime;
      int j;
      List<int> primes = new List<int>();

      primes.Add(2);

      while (counter <= upperLimit) {
        j = 0;
        isPrime = true;
        while (primes[j] * primes[j] <= counter) {
          if (counter % primes[j] == 0) {
            isPrime = false;
            break;
          }
          j++;
        }
        if (isPrime)
          primes.Add(counter);

        counter += 2;
      }

      return primes.ToArray();
    }

#endregion

#region Sieve of Erastosthenes
    public void SieveOfEratosthenes() {
      DateTime startTime = DateTime.Now;

      int[] primes = ESieve(numm);

      decimal primeSum = 0;

      for (int i=0; i<primes.Length; i++)
        primeSum += primes[i];

      DateTime stopTime = DateTime.Now;
      TimeSpan duration = stopTime - startTime;
      Console.WriteLine("Prime sum of all primes below {0} is {1} ", numm, primeSum);
      Console.WriteLine("Solution took {0} ms using Sieve of Eratosthenes", duration.TotalMilliseconds);
    }

    public int[] ESieve(int upperLimit) {
      int sieveBound = (int)(upperLimit - 1) / 2;
      int upperSqrt  = ((int)Math.Sqrt(upperLimit) - 1) / 2;

      BitArray PrimeBits = new BitArray(sieveBound + 1, true);

      for (int i = 1; i <= upperSqrt; i++) {
        if (PrimeBits.Get(i)) {
          for (int j = i * 2 * (i + 1); j <= sieveBound; j += 2 * i + 1) {
            PrimeBits.Set(j, false);
          }
        }
      }

      List<int> numbers = new List<int>((int)(upperLimit / (Math.Log(upperLimit) - 1.08366)));
      numbers.Add(2);
      for (int i = 1; i <= sieveBound; i++) {
        if (PrimeBits.Get(i)) {
          numbers.Add(2 * i + 1);
        }
      }

      return numbers.ToArray();
    }   
#endregion

#region Sieve of Atkin
    public void SieveOfAtkin() {
      DateTime startTime = DateTime.Now;


      int[] primes = ASieve(numm);

      decimal primeSum = 0;

      for (int i = 0; i < primes.Length; i++) {
        primeSum += primes[i];
      }

      DateTime stopTime = DateTime.Now;
      TimeSpan duration = stopTime - startTime;
      Console.WriteLine("Prime sum of all primes below {0} is {1} ", numm, primeSum);
      Console.WriteLine("Solution took {0} ms using Sieve of Atkin", duration.TotalMilliseconds);
    }

    public int[] ASieve(int upperLimit) {
      BitArray PrimeBits = new BitArray(upperLimit + 1, false);
      int upperSqrt = (int)Math.Sqrt(upperLimit);

      for (int i = 1; i <= upperSqrt; i++) {
        for (int j = 1; j <= upperSqrt; j++) {

          int n = 4 * i * i + j * j;
          if (n <= upperLimit && (n % 12 == 1 || n % 12 == 5)) {
            PrimeBits.Set(n, !PrimeBits.Get(n));
          }
          n = 3 * i * i + j * j;
          if (n <= upperLimit && (n % 12 == 7)) {
            PrimeBits.Set(n, !PrimeBits.Get(n));
          }
          n = 3 * i * i - j * j;
          if (i > j && n <= upperLimit && (n % 12 == 11)) {
            PrimeBits.Set(n, !PrimeBits.Get(n));
          }
        }
      }

      for (int i = 5; i <= upperSqrt; i++) {
        if (PrimeBits.Get(i)) {
          for (int j = i * i; j <= upperLimit; j += i * i) {
            PrimeBits.Set((int)j, false);
          }
        }
      }

      List<int> numbers = new List<int>();
      numbers.Add(2);
      numbers.Add(3);
      for (int i = 5; i <= upperLimit; i += 2) {
        if (PrimeBits.Get(i)) {
          numbers.Add(i);
        }
      }

      return numbers.ToArray();
    }

#endregion
  }
}
