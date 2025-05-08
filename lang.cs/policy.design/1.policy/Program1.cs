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

        Parallel.For(0, 1000, _ => { counter.Increment(); });

        Console.WriteLine("Sum: " + counter.GetValue());
    }
}
