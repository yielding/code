using System;
using System.Threading;
using System.Threading.Tasks;

public class Program
{
    private static Int32 Sum(Int32 n)
    {
        Int32 sum = 0;
        for (; n > 0; n--)
            checked { sum += n; } 

        return sum;
    }

    public static void Main() 
    {
        Task<Int32[]> parent = new Task<Int32[]>(() => {
            var results = new Int32[3];   // Create an array for the results

            // This tasks creates and starts 3 child tasks
            new Task(() => results[0] = Sum(100), TaskCreationOptions.AttachedToParent).Start();
            new Task(() => results[1] = Sum(200), TaskCreationOptions.AttachedToParent).Start();
            new Task(() => results[2] = Sum(300), TaskCreationOptions.AttachedToParent).Start();

            // Returns a reference to the array
            // (even though the elements may not be initialized yet)
            return results;
        });

        // When the parent and its children have
        // run to completion, display the results
        var cwt = parent.ContinueWith(childTask => Array.ForEach(childTask.Result, Console.WriteLine));

        Console.WriteLine("1. Ready");
        // Start the parent Task so it can start its children
        parent.Start();

        Console.WriteLine("2. Run");

        cwt.Wait();
    }
}
