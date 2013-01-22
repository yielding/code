using System;
using System.IO;
using System.Linq;
using System.Threading;

class Application
{
    public class TPTest
    {
        private int count;

        public TPTest(int count)
        {
            this.count = count;
        }

        public void Run()
        {
            for (int i=0; i<this.count; i++)
                ThreadPool.QueueUserWorkItem(new WaitCallback(ThreadProc));

            WaitCallback wi = (data) => { Console.WriteLine((string)data); };

            string xxx = "Data transferred to Thread";
            ThreadPool.QueueUserWorkItem(wi, xxx);
        }

        private void ThreadProc(Object stateInfo)
        {
            Console.WriteLine("Hello from the thread pool");
        }
    }

    public static int Main(string[] args)
    {
        var t = new TPTest(10);
        t.Run();

        Thread.Sleep(1000 * 2);

        return 0;
    }
}
