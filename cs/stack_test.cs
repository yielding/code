using System;
using Acme.Collections;

class Test 
{
  static void Main()
  {
    Stack s = new Stack();
    const int a = 10;

    s.Push(1);
    s.Push(10);
    s.Push(100);
    s.Push("leech");

    Console.WriteLine(s.Pop());
    Console.WriteLine(s.Pop());
    Console.WriteLine(s.Pop());
    Console.WriteLine(s.Pop());
  }
}
