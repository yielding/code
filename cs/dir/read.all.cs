using System;
using System.IO;
using System.Linq;
using System.Collections.Generic;

class Application
{
  public static int Main(string[] args)
  {
    var paths = Directory.GetFiles(@"/Users/yielding/code", "*.rb",
        SearchOption.AllDirectories);

     foreach (var path in paths)
     {
       Console.WriteLine(path);
     }
    
    return 0;
  }
}
