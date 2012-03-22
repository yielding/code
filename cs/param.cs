using System;

public class Param
{
  public static void p(string fmt, params object[] args)
  {
    Console.WriteLine(fmt, args);
  }

  public static void Main(string[] _args)
  {
    string s = "x = {0}, y = {1}, z = {2}";
    object[] args = new object[3];
    args[0] = 1;
    args[1] = 2;
    args[2] = 3;
    p(s, args);
  }
}
