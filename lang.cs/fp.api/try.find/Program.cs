using System;
using CSharpFunctionalExtensions;

public class Fp
{
  static void Main()
  {
    var fruits = new Dictionary<string, int> {
      { "apple", 10 },
      { "banana", 2 },
    };
    
    var apples = fruits.TryFind("apple");
    Console.WriteLine($"apples = {apples.Value}");

    var kiwi = fruits.TryFind("kiwi").Or(0);
    Console.WriteLine($"kiwis = {kiwi.Value}");

    kiwi.Match(
      fruit => Console.WriteLine($"It's a {fruit}"),
      () => Console.WriteLine("There's not fruit")
    );
  }
}
