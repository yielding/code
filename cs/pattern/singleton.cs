using System;
using System.IO;
using System.Linq;
using System.Collections.Generic;

public abstract class SingletonBase<T> where T : class
{
  /// <summary>
  /// Static instance. Needs to use lambda expression
  /// to construct an instance (since constructor is private).
  /// </summary>
  private static readonly Lazy<T> sInstance = new Lazy<T>(() => CreateInstanceOfT());



  /// <summary>
  /// Gets the instance of this singleton.
  /// </summary>
  public static T Instance { get { return sInstance.Value; } }



  /// <summary>
  /// Creates an instance of T via reflection since T's constructor is expected to be private.
  /// </summary>
  /// <returns></returns>
  private static T CreateInstanceOfT()
  {
    return Activator.CreateInstance(typeof(T), true) as T;
  }
}


public class Repo: SingletonBase<Repo>
{
  public void P()
  {
      Console.WriteLine("Hello world");
  }

  private Repo() { }
}


class Application
{
  public static int Main(string[] args)
  {
    Repo.Instance.P();
    
    return 0;
  }
}
