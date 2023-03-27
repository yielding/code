using System;
using System.IO;
using System.Linq;
using System.Collections.Generic;

class Application
{
  public static int Main(string[] args)
  {
    string s0 = "1, 2, 3, 4, 5";
    var nums = s0.Split(',');
    foreach (var no in nums) 
      Console.WriteLine(no);

    var s1 = String.Join(",", nums);
    Console.WriteLine(s1);

    return 0;
  }
}
