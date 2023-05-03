using System;
using System.IO;
using System.Threading;
using System.Threading.Tasks;

class Application
{
  private static int Sum(CancellationToken ct, int n)
  {
    int sum = 0;
    for (; n > 0; n--)
    {
      ct.ThrowIfCancellationRequested();

      checked { sum += n; }
    }

    return sum;
  }

  public static int Main(string[] args)
  {
    var cts = new CancellationTokenSource();
    var t = new Task<int>(() => Sum(cts.Token, 1000), cts.Token);
    t.Start();
    cts.Cancel();

    try 
    {
      Console.WriteLine("The sum is : {0}", t.Result);
    }
    catch(AggregateException ae)
    {
      ae.Handle(e => e is OperationCanceledException);
      Console.WriteLine("Sum was canceled");
    }

    return 0;
  }
}
