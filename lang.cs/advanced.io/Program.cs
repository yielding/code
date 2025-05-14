using System;
using System.Diagnostics;
using System.IO;
using System.IO.MemoryMappedFiles;
using System.Threading;
using System.Threading.Tasks;

class Program
{
    const string FilePath = "/Users/yielding/code/lang.cs/advanced.io/1GB_file";
    const int ThreadCount = 16;
    const int ReadSize = 64 * 1024; // 64KB
    const int ReadsPerThread = 100_000;

    static void Main()
    {
        Console.WriteLine("==== Starting Benchmark ====\n");

        RunMMFBenchmark();
        Console.WriteLine();
        RunFileStreamWithLockBenchmark();
    }

    static void RunMMFBenchmark()
    {
        Console.WriteLine(">>> MemoryMappedFile (lock-free)");

        long fileLength = new FileInfo(FilePath).Length;
        long totalBytes = 0;
        var sw = Stopwatch.StartNew();

        using var mmf = MemoryMappedFile.CreateFromFile(FilePath, FileMode.Open, null, 0, MemoryMappedFileAccess.Read);
        using var accessor = mmf.CreateViewAccessor(0, 0, MemoryMappedFileAccess.Read);

        Parallel.For(0, ThreadCount, threadId =>
        {
            var rnd = new Random(threadId ^ Environment.TickCount);
            byte[] buffer = new byte[ReadSize];

            for (int i = 0; i < ReadsPerThread; i++)
            {
                long offset = ((long)rnd.NextInt64(fileLength - ReadSize) / ReadSize) * ReadSize;
                accessor.ReadArray(offset, buffer, 0, ReadSize);
                Interlocked.Add(ref totalBytes, ReadSize);
            }
        });

        sw.Stop();
        PrintStats(sw.Elapsed.TotalSeconds, totalBytes);
    }

    static void RunFileStreamWithLockBenchmark()
    {
        Console.WriteLine(">>> FileStream + lock");

        long fileLength = new FileInfo(FilePath).Length;
        long totalBytes = 0;
        object fileLock = new object();
        var sw = Stopwatch.StartNew();

        using var fs = new FileStream(FilePath, FileMode.Open, FileAccess.Read, FileShare.Read);

        Parallel.For(0, ThreadCount, threadId =>
        {
            var rnd = new Random(threadId ^ Environment.TickCount);
            byte[] buffer = new byte[ReadSize];

            for (int i = 0; i < ReadsPerThread; i++)
            {
                long offset = ((long)rnd.NextInt64(fileLength - ReadSize) / ReadSize) * ReadSize;

                lock (fileLock)
                {
                    fs.Seek(offset, SeekOrigin.Begin);
                    fs.Read(buffer, 0, ReadSize);
                }

                Interlocked.Add(ref totalBytes, ReadSize);
            }
        });

        sw.Stop();
        PrintStats(sw.Elapsed.TotalSeconds, totalBytes);
    }

    static void PrintStats(double seconds, long totalBytes)
    {
        double mbRead = totalBytes / (1024.0 * 1024.0);
        double throughput = mbRead / seconds;

        Console.WriteLine($"Threads        : {ThreadCount}");
        Console.WriteLine($"Read Size      : {ReadSize / 1024} KB");
        Console.WriteLine($"Total Reads    : {ThreadCount * ReadsPerThread}");
        Console.WriteLine($"Total Data     : {mbRead:F2} MB");
        Console.WriteLine($"Elapsed Time   : {seconds:F2} s");
        Console.WriteLine($"Throughput     : {throughput:F2} MB/s");
    }
}