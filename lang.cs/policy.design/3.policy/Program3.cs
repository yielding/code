public interface ILockingPolicy 
{
    void Enter();
    void Exit();
}

public class NoLockingPolicy : ILockingPolicy 
{
    public void Enter() { }
    public void Exit() { }
}

public class MonitorLockingPolicy : ILockingPolicy 
{
    private readonly object _lock = new object();

    public void Enter() 
    {
        Monitor.Enter(_lock);
    }

    public void Exit() 
    {
        Monitor.Exit(_lock);
    }
}

public class Counter <TPolicy> 
    where TPolicy : ILockingPolicy, new() 
{
    private int _count = 0;
    private readonly TPolicy _lockPolicy = new TPolicy();  // 이 언어의 한계

    public void Increment() 
    {
        _lockPolicy.Enter();
        
        try { _count++; } finally { _lockPolicy.Exit(); }
    }

    public int GetValue() 
    {
        _lockPolicy.Enter();

        try { return _count; } finally { _lockPolicy.Exit(); }
    }
}

class Program 
{
    static void Main(string[] args) 
    {
        var counter1 = new Counter<NoLockingPolicy>();
        var counter2 = new Counter<MonitorLockingPolicy>();

        Parallel.For(0, 1000, _ => { counter1.Increment(); });
        Parallel.For(0, 1000, _ => { counter2.Increment(); });

        Console.WriteLine("NoLockingPolicy: " + counter1.GetValue());
        Console.WriteLine("MonitorLockingPolicy: " + counter2.GetValue());
    }
}
