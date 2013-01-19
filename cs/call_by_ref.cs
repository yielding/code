using System;

public class CSharpApp
{
    public static void call_by_value(ref int b)
    {
        b = 11;
    }

    static void Main()
    {
        int[] a = {1, 2, 3, 4, 5 };

        int b = 10;
        call_by_value(ref b);

        Console.WriteLine(b);
    }
}
