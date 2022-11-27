using System;
using System.Threading;

public class ThreadTest {

  public class MyJob {
    public int repetitions = 1;
    public void run_me() {
      for (int i=0; i<repetitions; i++) {
        Thread.Sleep(1000);
        Console.WriteLine(Thread.CurrentThread.Name);
      }
    }
  }

  public static void Main(string[] args) {
    var j1 = new MyJob();
    j1.repetitions = 3;

    Thread thread1 = new Thread(new ThreadStart(j1.run_me));
    thread1.Name = "first";
    thread1.Start();

    var j2 = new MyJob();
    j2.repetitions = 5;
    Thread thread2 = new Thread(new ThreadStart(j2.run_me));
    thread2.Name = "second";
    thread2.Start();
  }
}
