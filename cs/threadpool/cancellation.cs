using System;
using System.IO;
using System.Linq;
using System.Threading;

public class Application
{
    public class TPCancellation
    {
        private int count;

        public TPCancellation(int count)
        {
            this.count = count;
        }

        public void Run()
        {
            var cts = new CancellationTokenSource();

            for (int i=0; i<10; i++)
            {
                WaitCallback wc = (s) => {
                    var token = (CancellationTokenSource)s;
                    for (int j=0; j<50; j++)
                    {
                        if (token.IsCancellationRequested)
                            return;

                        Console.WriteLine("Output");
                        token.WaitHandle.WaitOne(1000);
                    }
                };

                ThreadPool.QueueUserWorkItem(wc, cts.Token);

                cts.Cancel();
            }
        }
    }

    public static int Main(string[] args)
    {
        var t = new TPCancellation(10);
        t.Run();

        Thread.Sleep(1000 * 2);

        return 0;
    }
}
