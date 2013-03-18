using System;
using System.IO;
using System.Linq;
using System.Collections.Generic;

class MainApp
{
    public static void Main(string[] args)
    {
        var xys = new List<Tuple<int, int>>();

        xys.Add(new Tuple<int, int>(1, 2));
        xys.Add(Tuple.Create(1, 2));
        xys.Add(Tuple.Create(1, 2));

        var xx = Tuple.Create(3, 4);
        xys.Add(xx);

        Console.WriteLine(xx);
        Console.WriteLine(xx.Item1);
        Console.WriteLine(xx.Item2);

        foreach (var i in xys) Console.WriteLine(i);
    }
}
