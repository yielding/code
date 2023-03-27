using System;
using System.IO;
using System.Text;

class CSharpApp
{
  public void BinFileIO(string path)
  {
    try
    {
      if (File.Exists(path))
        File.Delete(path);

      var fs0 = new FileStream(path, FileMode.Create, FileAccess.ReadWrite);
      using (var bw = new BinaryWriter(fs0))
      {
        // int    one    = 1;
        // double dl_one = 0.01;
        // REMARK
        // the first byte of string is its "length"
        //
        // format: length, utf8
        string s = "l";  
        var res = "";

        for(int i=0; i<2000; i++)
          res += s;


        // bw.Write(one); bw.Write(dl_one); 
        bw.Write(res);

        /*
           var utf8  = System.Text.Encoding.UTF8;
           var bytes = utf8.GetBytes(s);
           bw.Write(bytes);

           foreach(var b in bytes)
           Console.Write("{0:X} ", b);
           */
      }

      /*
         var fs1 = new FileStream(path, FileMode.Open, FileAccess.ReadWrite);
         using (var br = new BinaryReader(fs1))
         {
         var one    = br.ReadInt32();
         var bl_one = br.ReadDouble();
         var str    = br.ReadString();

         Console.WriteLine("{0}, {1}, {2}", 
         one.ToString(), bl_one.ToString(), str);
         }
         */
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
    // app.TextFileIO(path);
    app.BinFileIO(path);
  }
}
