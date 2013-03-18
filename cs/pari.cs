using System;
using System.Collections.Generic;

class MainApp
{
    public static void Main(string[] args)
    {
        var t0 = new Tuple<string, int>("Hello", 4);
        Console.WriteLine(t0.Item1);
        Console.WriteLine(t0.Item2);

        var t1 = Tuple.Create("Hello2", 5);
        Console.WriteLine(t1.Item1);
        Console.WriteLine(t1.Item2);
    }
}
