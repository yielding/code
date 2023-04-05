using System;
using System.Collections.Concurrent;
using System.Runtime.InteropServices;
using System.Threading;
using System.Threading.Tasks;

class ProdoucerConsumer
{
  private readonly int inputs = 2000;
  private CancellationTokenSource ct;
  private BlockingCollection<int> queue;
  private int capacity;

  public CancellationToken CancelToken { get => ct.Token; }
  public BlockingCollection<int> Queue { get => queue; }

  public ProdoucerConsumer(int capacity)
  {
    this.capacity = capacity;

    ct    = new CancellationTokenSource();
    queue = new BlockingCollection<int>(capacity);
  }

  public void Shutdown() { ct.Cancel(); }

  public void Complete()
  {
    queue.CompleteAdding();
  }

  public void Run()
  {
    // TODO
    // Task.WaitAll(Task.Run(() => Consumer()), Task.Run(() => Producer()));
    Task.WaitAll(Task.Run(() => Consumer()), 
                 Task.Run(() => Consumer()));
  }

  void Consumer()
  {
    while (!queue.IsCompleted)
    {
      int nextItem = 0;
      try
      {
        if (!queue.TryTake(out nextItem, -1, ct.Token))
        {
          Console.WriteLine("Something wrong happened");
          return;
        }

        Console.WriteLine(" Take:{0}", nextItem);
      }
      catch (OperationCanceledException)
      {
        Console.WriteLine("Taking canceled.");
        break;
      }

      doWork();
      // REAMRK
      // : consumers work here!
      // Slow down consumer just a little to cause
      // collection to fill up faster, and lead to "AddBlocked"
      Thread.SpinWait(500000);
    }

    Console.WriteLine("\r\nNo more items to take.");
  }

  protected virtual void doWork()
  {
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
        success = queue.TryAdd(itemToAdd, 2, ct.Token);
      }
      catch (OperationCanceledException)
      {
        Console.WriteLine("Add loop canceled.");
        // Let other threads know we're done in case
        // they aren't monitoring the cancellation token.
        queue.CompleteAdding();
        break;
      }

      if (success)
      {
        Console.WriteLine(" Add:{0}", itemToAdd);
        itemToAdd++;
      }
      else
      {
        Console.Write(" AddBlocked:{0} Count = {1}", itemToAdd.ToString(), queue.Count);
        // Don't increment nextItem. Try again on next iteration.

        //Do something else useful instead.
        // UpdateProgress(itemToAdd);
      }
      Thread.Sleep(1000);
    } while (itemToAdd < inputs);

    // No lock required here because only one producer.
    queue.CompleteAdding();
  }
}

class ProgramWithCancellation
{
  static void Main()
  {
    var pc = new ProdoucerConsumer(100);
    var queue = pc.Queue;
    var token = pc.Token;

    Task.Run(() => { pc.Run(); });

    queue.TryAdd(item, -1, token);


  }
}