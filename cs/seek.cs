using System;
using System.IO;
using System.Text;

class CSharpApp
{
  public void BinFileIO(string path)
  {
    try
    {
      var fs0 = new FileStream(path, FileMode.Open, FileAccess.Read);

      fs0.Seek(2, SeekOrigin.End);
      var buffer = new byte[2];
      fs0.Read(buffer, 0, 2);
      Console.WriteLine(buffer[0]);
      Console.WriteLine(buffer[1]);
    }
    catch(Exception e)
    {
      Console.WriteLine("The process failed: {0}", e.ToString());
    }
  }

  static void Main(string[] args)
  {
    string path = "/Users/yielding/code/cs/file_io.bin";
    var app = new CSharpApp();
    app.BinFileIO(path);
  }
}
