using System;
using System.IO;
using System.Linq;
using System.Collections.Generic;

class CSharpApp
{
  static void Main(string[] args)
  {
    var names = new List<string>() 
    {
      "leech", "kamin", "gunhee", "gunhee2"
    };

    var nameList = names.Where(name => name.StartsWith("g"))
                        .Aggregate("", (all, next) => all + ", " + next);

    Console.WriteLine(nameList);
  }
}
