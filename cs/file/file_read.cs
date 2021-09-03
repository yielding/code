using System;
using System.IO;
using System.Text;

class CSharpApp
{
  public void BinFileIO(string path)
  {
    try
    {
      var buffer = new byte[0x2000];
      // Garbage collector (python, ruby, java, c#, rust, go) <-> c/c++
      // C, LISP
      // managed / unmanaged
      // endian
      // signed index
      using (var fs = new FileStream(path, FileMode.Open, FileAccess.Read))
      {
        int r = fs.Read(buffer, 0, 0x2000);

        Console.WriteLine("buffer size: {0}, file size: {1}", buffer.Length, r);
      }
    }
    catch(Exception e)
    {
      Console.WriteLine("The process failed: {0}", e.ToString());
    }
  }

  static void Main(string[] args)
  {
    string path = "/Users/yielding/code/cs/file_read.cs";
    var app = new CSharpApp();
    app.BinFileIO(path);
  }
}
