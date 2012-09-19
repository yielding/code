using System;
using System.IO;

class CSharpApp
{
    public void TextFileIO(string path)
    {
        try
        {
            if (File.Exists(path))
                File.Delete(path);

            using (StreamWriter sw = new StreamWriter(path))
            {
                sw.WriteLine("This");
                sw.WriteLine("is some text");
                sw.WriteLine("to test");
                sw.WriteLine("Reading");
            }

            using (StreamReader sr = new StreamReader(path))
            {
                while (sr.Peek() > 0)
                    Console.WriteLine(sr.ReadLine());
            }
        }
        catch(Exception e)
        {
            Console.WriteLine("The process failed: {0}", e.ToString());
        }
    }

    public void BinFileIO(string path)
    {
        try
        {
            if (File.Exists(path))
                File.Delete(path);

            FileStream fs = new FileStream(path, FileMode.Create, FileAccess.ReadWrite);
            using (BinaryWriter bw = new BinaryWriter(fs))
            {
                int one = 1;
                double dl_one = 0.01;
                string s = "Hello C#";
                bw.Write(one);
                bw.Write(dl_one);
                bw.Write(s);
            }

            FileStream fs1 = new FileStream(path, FileMode.Open, FileAccess.ReadWrite);
            using (BinaryReader br = new BinaryReader(fs1))
            {
                var one    = br.ReadInt32();
                var bl_one = br.ReadDouble();
                var str    = br.ReadString();

                Console.WriteLine("{0}, {1}, {2}", 
                        one.ToString(), bl_one.ToString(), str);
            }
        }
        catch(Exception e)
        {
            Console.WriteLine("The process failed: {0}", e.ToString());
        }
    }

    static void Main(string[] args)
    {
        string path = "/Users/yielding/code/cs/file_io.txt";
        var app = new CSharpApp();
        app.TextFileIO(path);
        app.BinFileIO(path);
    }
}
