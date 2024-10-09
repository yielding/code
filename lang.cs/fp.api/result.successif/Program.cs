using CSharpFunctionalExtensions;

class FruitInventory
{
    public string Name;
    public int Count;

    public static FruitInventory Create(string name, int count)
    {
        return new FruitInventory(name, count);
    }

    public FruitInventory(string name, int count)
    {
        Name = name;
        Count = count;
    }

    public override string ToString()
    {
        return $"({Name}, {Count})";
    }
}

class Program
{
    static void SuccessIf()
    {
        var successful = false;
        Result found = Result.SuccessIf(successful, "it is not successful");
        Console.WriteLine(found);
    }

    static void Tap()
    {
        var appleInventory = Result.Success(new FruitInventory("apple", 4))
                                .Tap(fi => Console.WriteLine(fi))      // should (apple, 4)
                                .Tap(fi => Console.WriteLine("hi"));   // should hi

        Console.WriteLine($"content: {appleInventory.Value}");
        
        var failedOperation = Result.Failure<FruitInventory>("Could not find inventory")
                                .Tap(fi => Console.WriteLine(fi.Name))  // should ""
                                .Tap(fi => Console.WriteLine("hi"));    // should ""
        
        Console.WriteLine(failedOperation.Error);
    }

    static void SuccessWithoutValue()
    {
        Result successInventoryUpdate = Result.Success();
        // Console.WriteLine(successInventoryUpdate.Value); Error
    }

    static void Main(string[] args)
    {
        var fi = Result.Of(FruitInventory.Create("banana", 3));
        Console.WriteLine(fi);
    }
}
