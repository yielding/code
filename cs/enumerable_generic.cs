using System;
using System.Collections;
using System.Collections.Generic;

namespace EnumerableGeneric
{
  class MyList<T>: IEnumerable<T>, IEnumerator<T>
  {
    private T[] array;
    int position = -1;

    public MyList()
    {
      array = new T[3];
    }

    public T this[int index]
    {
      get => array[index];

      set {
        if (index >= array.Length) {
          Array.Resize<T>(ref array, index + 1);
          Console.WriteLine("Array Resized : {0}", array.Length);
        }

        array[index] = value;
      }
    }

    public int Length
    {
      get => array.Length;
    }

    public IEnumerator<T> GetEnumerator()
    {
      for (int i=0; i<array.Length; i++)
        yield return array[i];
    }

    IEnumerator IEnumerable.GetEnumerator()
    {
      for (int i=0; i<array.Length; i++)
        yield return array[i];
    }

    public T Current
    {
      get => array[position];
    }

    object IEnumerator.Current
    {
      get => array[position];
    }

    public bool MoveNext()
    {
      if (position == array.Length - 1)
      {
        Reset();
        return false;
      }

      position++;
      return position < array.Length;
    }

    public void Reset()
    {
      position = -1;
    }

    public void Dispose()
    {
    }
  }

  class CSharpApp
  {
    static void Print<T>(T value)
    {
      Console.WriteLine("{0} ", value);
    }

    static void Main(string[] args)
    {
      var str_list = new MyList<string>();

      str_list[0] = "abc";
      str_list[1] = "def";
      str_list[2] = "ghi";
      str_list[3] = "jkl";
      str_list[4] = "mno";

      foreach (var str in str_list)
        Console.WriteLine(str);

      var int_list = new MyList<int>();
      int_list[0] = 1;
      int_list[1] = 2;
      int_list[2] = 3;
      int_list[3] = 4;
      int_list[4] = 5;
      int_list[5] = 6;

      foreach (var i in int_list)
        Console.WriteLine(i);
    }
  }
}
