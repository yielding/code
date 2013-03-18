using System;
using System.IO;
using System.Linq;
using System.Collections.Generic;

class Printer 
{
    public void Print()
    {
        DoPrint();
    }

    protected virtual void DoPrint()
    {
    }
}

class LaserPrinter: Printer
{
    protected override void DoPrint()
    {
        Console.WriteLine("I'm LaserPrinter");
    }

}

class InkJetPrinter: Printer
{
    protected override void DoPrint()
    {
        Console.WriteLine("I'm InkJetPrinter");
    }
}

class Application
{

    public static int Main(string[] args)
    {
        var p0 = new InkJetPrinter();
        p0.Print();

        var p1 = new LaserPrinter();
        p1.Print();
        
        return 0;
    }
}
