using System;
using System.Collections;

namespace CSharpApp
{
  class MainApp
  {
    static void Main(string[] args)
    {
      Hashtable ht = new Hashtable();
      ht["하나"] = "one";
      Console.WriteLine(ht["하나"]);
      Console.WriteLine(ht["둘"]);
    }
  }
}
