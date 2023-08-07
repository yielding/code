using System;
using System.IO;
using System.Diagnostics;

namespace StdioRedirect
{
  public class CommandHelper
  {
    private Stream origin;
    private Stream converted;

    public CommandHelper(Stream input, Stream output)
    {
      origin = input;
      converted = output;
    }

    public bool Run(string program)
    {
      var si = new ProcessStartInfo {
        CreateNoWindow = true,
        UseShellExecute = false,
        FileName = program,
        Arguments = "",
        RedirectStandardInput = true,
        RedirectStandardOutput = true,
        RedirectStandardError = true
      };

      var ct = new CancellationTokenSource();

      try
      {
        using (var p = Process.Start(si))
        {
          string eOut="";
          p.EnableRaisingEvents = true;
          p.ErrorDataReceived += (sender, e) => eOut += e.Data;
          p.BeginErrorReadLine();
          origin.Position = 0;
          var externalTask = 
            origin.CopyToAsync(p.StandardInput.BaseStream, 81920, ct.Token)
                  .ContinueWith((t) => p.StandardInput.Close(), ct.Token);

          p.StandardOutput.BaseStream.CopyTo(converted);
          p.WaitForExit();
          externalTask.Wait();
          return p.ExitCode != 1;
        }
      }
      catch (Exception e)
      {
        Console.WriteLine(e.Message);
      }

      return false;
    }
  }

  public class Example
  {
    public static void Main()
    {
      var ipath = "/Users/yielding/Desktop/IMG_1078.JPG";
      using (var img = File.Open(ipath, FileMode.OpenOrCreate))
      using (var mem = new MemoryStream())
      {
        var cmd = new CommandHelper(img, mem);
        if (!cmd.Run("cpp.stdio"))
        {
          Console.WriteLine("Error happened");
          return;
        }

        Console.WriteLine("size of out: {0}", mem.Length);

        var opath = "/Users/yielding/Desktop/res.jpg";
        using (var res = new FileStream(opath, FileMode.Create, FileAccess.Write))
        {
          //var bytes = new byte[mem.Length];
          mem.Position = 0;
          mem.CopyTo(res);
        }
      }
    }
  }
}
