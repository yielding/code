using System;

namespace LambdaTest
{
  class CSharpApp
  {
    static void Main(string[] args)
    {
      Action act1 = () => Console.WriteLine("Action()");
      act1();

      int result = 0;
      Action<int> act2 = (x) => result = x * x;
      act2(3);
      Console.WriteLine("reuslt : {0}", result);

      Action<double, double> act3 = (x, y) => {
        double pi = x / y;
        Console.WriteLine("Action<T1, T2>({0}, {1}) : {2})", x, y, pi);
      };

      act3(22.0, 7.0);

      Func<int, int, int> binary = (a, b) => a + b;
      Console.WriteLine("1 + 2 = {0}", binary(1, 2));
    }
  }
}
