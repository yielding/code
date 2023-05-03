using System;
using GMDInteger;

namespace GMDInteger 
{
  public static class IntegerExtension
  {
    public static int Square(this int myInt)
    {
      return myInt * myInt;
    }

    public static int Power(this int myInt, int exponent)
    {
      int result = myInt;
      for (int i=0; i<exponent; i++)
        result *= myInt;

      return result;
    }
  }
}

namespace CSharpApp
{
  class MainApp
  {
    static void Main(string[] args)
    {
      Console.WriteLine("{0} ^ 2: {1}", 3, 3.Square());
    }
  }
}
