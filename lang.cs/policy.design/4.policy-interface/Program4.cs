public interface ILockingPolicy {
    void Enter();
    void Exit();
}

public class NoLockingPolicy : ILockingPolicy {
    public void Enter() { }
    public void Exit() { }
}

public class MonitorLockingPolicy : ILockingPolicy {
    private readonly object _lock = new object();

    public void Enter() {
        Monitor.Enter(_lock);
    }

    public void Exit() {
        Monitor.Exit(_lock);
    }
}

public class Counter 
{
    private int _count = 0;
    private readonly ILockingPolicy _lockPolicy;

    public Counter(ILockingPolicy lockPolicy)
    {
        _lockPolicy = lockPolicy;
    }

    public void Increment() 
    {
        _lockPolicy.Enter();

        try 
        {
            _count++;
        } 
        finally 
        {
            _lockPolicy.Exit();
        }
    }

    public int GetValue() 
    {
        _lockPolicy.Enter();

        try 
        {
            return _count;
        } 
        finally 
        {
            _lockPolicy.Exit();
        }
    }
}

class Program 
{
    static void Main(string[] args) 
    {

        var counter1 = new Counter(new NoLockingPolicy());
        var counter2 = new Counter(new MonitorLockingPolicy());

        Parallel.For(0, 1000, _ => { counter1.Increment(); });
        // 병렬 실행 테스트
        Parallel.For(0, 1000, _ => { counter2.Increment(); });

        Console.WriteLine("NoLockingPolicy: " + counter1.GetValue());      // 항상 0
        Console.WriteLine("MonitorLockingPolicy: " + counter2.GetValue()); // 1000
    }
}

