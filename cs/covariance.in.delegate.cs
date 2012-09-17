using System;
using System.Collections.Generic;

public class CSharpApp
{
    static void Main()
    {
        Action<string> del = ShowMessage;
        del("Proximity alert");
    }

    static void ShowMessage(object message)
    {
        Console.WriteLine(message);
    }
}
