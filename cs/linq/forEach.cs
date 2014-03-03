using System;
using System.IO;
using System.Collections.Generic;

class CSharpApp
{
  static void Main(string[] args)
  {
    List<int> list = new List<int>();
    list.Add(1);
    list.Add(2);
    list.Add(3);

    list.ForEach(n => Console.WriteLine(n));
  }
}
