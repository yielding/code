using System;
using System.Collections.Generic;

public class CSharpApp
{
  static unsafe void Main()
  {
    int length = 10;

    int* arr = stackalloc int [length];

    *(arr + 0) = 1;
    *(arr + 1) = 2;
    *(arr + 2) = 3;

    for (int i=0; i<3; i++)
      Console.WriteLine(arr[i]);
  }
}
