using System;

public class CSharpApp
{
  static void Main()
  {
    int[] a = { 1, 2, 3, 4, 5 };
    int[] b = new int[3];

    // a = source array
    // 1 = start index in source array
    // b = destination array
    // 0 = start index in destination array
    // 3 = elements to copy
    Array.Copy(a, 1, b, 0, 3);

    for (int i=0; i<b.Length; i++) 
      Console.WriteLine(b[i]);

    foreach (var i in b) 
      Console.WriteLine(i);
  }
}
