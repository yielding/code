using System;
using System.IO;
using System.Linq;
using System.Collections.Generic;

class CSharpApp
{
  static void Main(string[] args)
  {
    byte[] bytes = new byte[] { 3, 1, 5, 3, 4, 5 };

    var l0 = Enumerable.TakeWhile(bytes, x => x % 2 == 1);
    foreach (var i in l0) Console.WriteLine(i);

    var arr = new List<int>() { 1, 2, 3, 4, 5 };
    var l1  = Enumerable.TakeWhile(arr, x => x % 2 == 1);
  }
}
