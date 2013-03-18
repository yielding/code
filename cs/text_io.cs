using System;
using System.IO;
using System.Text;

class CSharpApp
{
    public void TextFileIO(string path)
    {
        try
        {
            var fs0 = new FileStream(path, FileMode.Open, FileAccess.ReadWrite);
            fs0.Seek(0, SeekOrigin.End);
            using (var bw = new StreamWriter(fs0))
            {

                string line = "algebra babo-----------------------------------------------------------";
                Console.WriteLine(line);

                bw.WriteLine(line);
                Console.WriteLine("ok");
            }
        }
        catch(Exception e)
        {
            Console.WriteLine("The process failed: {0}", e.ToString());
        }
    }

    static void Main(string[] args)
    {
        string path = "/Users/yielding/Desktop/data.txt";
        var app = new CSharpApp();
        app.TextFileIO(path);
    }
}
