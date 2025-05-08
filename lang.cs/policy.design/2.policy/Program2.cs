public class Counter
{
    private int _count = 0;
    private readonly object _lock = new object();

    public void Increment() 
    {
        lock (_lock) 
        {
            _count++;
        }
    }

    public int GetValue() 
    {
        lock (_lock) 
        {
            return _count;
        }
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

