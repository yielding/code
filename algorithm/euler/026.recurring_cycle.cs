using System.Diagnostics;
using System.Collections.Generic;
using System;

namespace euler
{
  class Problem26
  {
    public static void Main(string[] args)
    {
      new Problem26().BruteForce();            
    }

    public void BruteForce()
    {
      Stopwatch clock = Stopwatch.StartNew();

      int sequenceLength = 0;
      int num = 0;

      for (int i=1000; i > 1; i--) {
        if (sequenceLength >= i) 
          break;

        int[] foundRemainders = new int[i];
        int value = 1;
        int position = 0;

        while (foundRemainders[value] == 0 && value != 0)
        {
          foundRemainders[value] = position;
          value *= 10;
          value %= i;
          position++;
        }

        if (position - foundRemainders[value] > sequenceLength)
        {
          num = i;
          sequenceLength = position - foundRemainders[value];                    
        }
      }

      clock.Stop();

      Console.WriteLine("The number with the longest recurring cycle is {0}, and the cycle is length is {1}", num, sequenceLength);
      Console.WriteLine("Solution took {0} ms", clock.ElapsedMilliseconds);
    }
  }
}
