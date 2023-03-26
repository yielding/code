using System;
using System.Collections.Concurrent;
using System.Runtime.InteropServices;
using System.Threading;
using System.Threading.Tasks;

class ProgramWithCancellation
{
  static int inputs = 2000;

  static void Main()
  {
    var ct = new CancellationTokenSource();

    // A blocking collection that can hold no more than 100 items at a time.
    var numbers = new BlockingCollection<int>(100);

    Task.Run(() => {
      if (Console.ReadKey(true).KeyChar == 'c') ct.Cancel();
    });

    // Wait for the tasks to complete execution
    Task.WaitAll(Task.Run(() => NonBlockingConsumer(numbers, ct.Token)),
                 Task.Run(() => NonBlockingProducer(numbers, ct.Token)));

    ct.Dispose();
    Console.WriteLine("Press the Enter key to exit.");
    Console.ReadLine();
  }

  static void NonBlockingConsumer(BlockingCollection<int> bc, CancellationToken ct)
  {
    // TODO REMARK
    // IsCompleted == (IsAddingCompleted && Count == 0)
    while (!bc.IsCompleted)
    {
      int nextItem = 0;
      try
      {
        if (!bc.TryTake(out nextItem, 0, ct))
          Console.WriteLine(" Take Blocked");
        else
          Console.WriteLine(" Take:{0}", nextItem);
      }
      catch (OperationCanceledException)
      {
        Console.WriteLine("Taking canceled.");
        break;
      }

      // Slow down consumer just a little to cause
      // collection to fill up faster, and lead to "AddBlocked"
      Thread.SpinWait(500000);
    }

    Console.WriteLine("\r\nNo more items to take.");
  }

  static void NonBlockingProducer(BlockingCollection<int> bc, CancellationToken ct)
  {
    int itemToAdd = 0;
    bool success = false;

    do
    {
      // Cancellation causes OCE. We know how to handle it.
      try
      {
        // A shorter timeout causes more failures.
        success = bc.TryAdd(itemToAdd, 2, ct);
      }
      catch (OperationCanceledException)
      {
        Console.WriteLine("Add loop canceled.");
        // Let other threads know we're done in case
        // they aren't monitoring the cancellation token.
        bc.CompleteAdding();
        break;
      }

      if (success)
      {
        Console.WriteLine(" Add:{0}", itemToAdd);
        itemToAdd++;
      }
      else
      {
        Console.Write(" AddBlocked:{0} Count = {1}", itemToAdd.ToString(), bc.Count);
        // Don't increment nextItem. Try again on next iteration.

        //Do something else useful instead.
        UpdateProgress(itemToAdd);
      }
    } while (itemToAdd < inputs);

    // No lock required here because only one producer.
    bc.CompleteAdding();
  }

  static void UpdateProgress(int i)
  {
    double percent = ((double)i / inputs) * 100;
    Console.WriteLine("Percent complete: {0}", percent);
  }
}
