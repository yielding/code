using System;
using System.IO;
using System.Linq;
using System.Collections.Generic;

public class Singleton<T> where T : class, new()
{
  public static T Instance { get; private set; }

  static Singleton()
  {
    if (Singleton<T>.Instance == null)
    {
      Singleton<T>.Instance = new T();
    }
  }

  public virtual void Clear()
  {
    Singleton<T>.Instance = null;
    Singleton<T>.Instance = new T();
  }
}

class MyStack: Singleton<MyStack>
{
  public MyStack()
  {
  }

  public void Push(int i)
  {
  }

  public int Pop()
  {
    return 0;
  }
}

class Application
{
  public static int Main(string[] args)
  {
    var s = MyStack.Instance;


    return 0;
  }
}
