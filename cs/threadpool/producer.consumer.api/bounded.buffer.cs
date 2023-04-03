using System;
using System.Collections.Concurrent;
using System.Runtime.InteropServices;
using System.Threading;
using System.Threading.Tasks;

class ProdoucerConsumer
{
  private readonly int inputs = 2000;
  private CancellationTokenSource ct;
  private BlockingCollection<int> numbers;
  private int capacity;

  public ProdoucerConsumer(int capa)
  {
    ct = new CancellationTokenSource();
    capacity = capa;
    numbers = new BlockingCollection<int>(capacity);
  }

  public void Shutdown()
  {
    ct.Cancel();
  }

  public void Run()
  {
    // Wait for the tasks to complete execution
    Task.WaitAll(Task.Run(() => Consumer()),
                 Task.Run(() => Producer()));

    ct.Dispose();
    Console.WriteLine("Press the Enter key to exit.");
    Console.ReadLine();
  }

  void Consumer()
  {
    while (!numbers.IsCompleted)
    {
      int nextItem = 0;
      try
      {
        if (!numbers.TryTake(out nextItem, 0, ct.Token))
          Console.WriteLine(" Take Blocked");
        else
          Console.WriteLine(" Take:{0}", nextItem);
      }
      catch (OperationCanceledException)
      {
        Console.WriteLine("Taking canceled.");
        break;
      }

      // REAMRK
      // : consumers work here!
      // Slow down consumer just a little to cause
      // collection to fill up faster, and lead to "AddBlocked"
      Thread.SpinWait(500000);
    }

    Console.WriteLine("\r\nNo more items to take.");
  }

  void Producer()
  {
    int itemToAdd = 0;
    bool success = false;

    do
    {
      // Cancellation causes OCE. We know how to handle it.
      try
      {
        // A shorter timeout causes more failures.
        success = numbers.TryAdd(itemToAdd, 2, ct.Token);
      }
      catch (OperationCanceledException)
      {
        Console.WriteLine("Add loop canceled.");
        // Let other threads know we're done in case
        // they aren't monitoring the cancellation token.
        numbers.CompleteAdding();
        break;
      }

      if (success)
      {
        Console.WriteLine(" Add:{0}", itemToAdd);
        itemToAdd++;
      }
      else
      {
        Console.Write(" AddBlocked:{0} Count = {1}", itemToAdd.ToString(), numbers.Count);
        // Don't increment nextItem. Try again on next iteration.

        //Do something else useful instead.
        // UpdateProgress(itemToAdd);
      }
    } while (itemToAdd < inputs);

    // No lock required here because only one producer.
    numbers.CompleteAdding();
  }
}

class ProgramWithCancellation
{
  static void Main()
  {
    var pc = new ProdoucerConsumer();
    pc.Run();
  }
}
