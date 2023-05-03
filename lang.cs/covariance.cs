using System;
using System.Collections.Generic;

public class CSharpApp
{
  static void Main()
  {
    IEnumerable<string> strings = new List<string>() {"1", "3", "2", "5" };

    PrintAll(strings);
  }

  static void PrintAll(IEnumerable<object> objects)
  {
    foreach (object o in objects)
    {
      System.Console.WriteLine(o);
    }
  }
}
