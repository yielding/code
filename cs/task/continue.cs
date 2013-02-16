using System;
using System.Threading.Tasks;

public class Program {
    private static Int32 Sum(Int32 n)
    {
        Int32 sum = 0;
        for (; n > 0; n--)
            checked { sum += n; } 
        return sum;
    }

    public static void Main() 
    {
        // Create Task, defer starting it, continue with another task
        var t = new Task<Int32>(n => Sum((Int32)n), 1000);
        t.Start();
        // notice the use of the Result property
        Task cwt = t.ContinueWith(task => Console.WriteLine("2. The sum is: " + task.Result));

        Console.WriteLine("1. here");

        cwt.Wait();
    }
}
