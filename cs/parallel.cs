using System;
using System.Collections.Generic;
using System.Threading;
using System.Threading.Tasks;

namespace ParallelLoop
{
  class CSharpApp
  {
    static bool IsPrime(long number)
    {
      if (number < 2)
        return false;

      if (number % 2 == 0 && number != 2)
        return false;

      for (long i=2; i<number; i++)
      {
        if (number % i == 0)
          return false;
      }

      return true;
    }

    static void Main(string[] args)
    {
      long from = Convert.ToInt64("10");
      long to   = Convert.ToInt64("10000000");

      Console.WriteLine("enter to start..");
      Console.ReadLine();
      Console.WriteLine("started ..");
      List<long> total = new List<long>();

      DateTime startTime = DateTime.Now;
      Parallel.For(from, to, (long i) => { if (IsPrime(i)) total.Add(i); });
      DateTime endTime = DateTime.Now;

      TimeSpan ellapsed = endTime - startTime;

      Console.WriteLine("Prime number count between {0} and {1} : {2}", 
          from, to, total.Count);
      Console.WriteLine("Ellapsed time : {0}", ellapsed);
    }
  }
}
