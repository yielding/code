using System;
using System.IO;
using System.Threading;
using System.Threading.Tasks;

class Application
{
    public static void T1()
    {
        var task = Task.Factory.StartNew(() => 
        {
            Thread.Sleep(1000);
            return "dummy value";
        });

        Console.WriteLine(task.Result);
    }

    public static void T2()
    {
        var task = Task.Factory.StartNew(() => 
        {
            Thread.Sleep(1000);
            return "dummy value";
        });

        task.ContinueWith(t => Console.WriteLine(t.Result)) 
            .ContinueWith(t => Console.WriteLine("We're done"));
    }

    public static void T3()
    {
        var t1 = Task.Factory.StartNew(() => 
        {
            Thread.Sleep(1000); return "dummy value1";
        });

        var t2 = Task.Factory.StartNew(() => 
        {
            Thread.Sleep(1000); return "dummy value2";
        });

        var t3 = Task.Factory.StartNew(() => 
        {
            Thread.Sleep(1000); return "dummy value3";
        });

        Task.Factory.ContinueWhenAll(new[] {t1, t2, t3}, tasks => {
            foreach(var task in tasks)
                Console.WriteLine(task.Result);
        });
    }

    public static int Main(string[] args)
    {
        T3();

        Thread.Sleep(4000);

        return 0;
    }
}
