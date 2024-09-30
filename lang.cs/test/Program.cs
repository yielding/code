using System;
using System.Collections.Generic;

namespace test
{
  class Program
  {
    static string GetString(int a)
    {
      return "a";
    }

    static List<string> GetString(int a, int b)
    {
      return new List<string>{"a"};
    }

    static void Main(string[] args)
    {
      GetString(1);
      GetString(1, 2);
      Console.WriteLine("hi");
    }

  }
}