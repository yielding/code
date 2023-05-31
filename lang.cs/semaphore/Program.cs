using System; 
using System.Threading; 

public class Example { 

  private static Semaphore _pool = null; 

  public static void Main() 
  { 
    Console.WriteLine("Run MD-Video"); 

    try 
    { 
      try
      {
        _pool = Semaphore.OpenExisting("MD-VIDEO"); 
      }
      catch(Exception e)
      {
        _pool = new Semaphore(initialCount: 2, maximumCount: 2, "MD-VIDEO"); 
      }

      if (_pool.WaitOne(1000) == false) 
      { 
        Console.WriteLine("MD-VIDEO가 이미 2개 실행중입니다."); 
        Console.ReadKey(); 
        return; 
      } 

      Console.WriteLine("MD-Video Starts");
      for (int i=0; i<100; i++)
      {
        Thread.Sleep(1000); 
        Console.WriteLine($"running {i} / 100");
      } 

      Console.ReadKey();
    } 
    finally 
    {
      _pool.Release(); 
    } 
  } 
}