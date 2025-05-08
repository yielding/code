class Counter
{
    private int _count = 0;

    public void Increment() 
    {
        _count++;
    }

    public int GetValue()
    {
        return _count;
    }
}

class Program 
{
    static void Main(string[] args) 
    {
        var counter = new Counter();
        for (int i=0; i<1000; i++)
            counter.Increment();

        Console.WriteLine("Sum: " + counter.GetValue());
    }
}

