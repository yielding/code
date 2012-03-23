using System;

class A
{
  ~A()
  {
    Console.WriteLine("Destructor of A");
  }
}

class B
{
  object Ref;

  public B(object o)
  {
    Ref = o;
  }

  ~B()
  {
    Console.WriteLine("Destructor of B");
  }
}

class Test
{
  static void Main()
  {
    B b = new B(new A());
    b = null;

    GC.Collect();
    GC.WaitForPendingFinalizers();
  }
}
