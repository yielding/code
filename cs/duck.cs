using System;

public class Duck
{
  public void quack()
  {
    Console.WriteLine("Quaack");
  }
}

public class Person
{
  public void quack()
  {
    Console.WriteLine("Person imitates a duck");
  }
}

public class CSharpApp
{
  static void Main()
  {
    var donald = new Duck();
    var josh   = new Person();
    InTheForest(donald);
    InTheForest(josh);
  }

  public static void InTheForest(dynamic duck)
  {
    duck.quack();
  }
}
