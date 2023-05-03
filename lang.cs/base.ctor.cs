using System;
using System.IO;
using System.Text;

public abstract class SQLiteFile
{
  public SQLiteFile(Stream s)
  {
    Execute(Stream s);
  }

  public abstract bool Execute(Stream s);
}

public class DBFile: SQLiteFile
{
  public static DBFile Create(path)
  {
    // var s = new File.OpenRead(path);
    var s = new MemoryStream();
    s.Write();
    return new DBFile(s);
  }

  public DBFile(Stream s) : base(s)
  {
    Console.WriteLine("DBFile.ctor");
  }

  public override bool Execute(Stream s)
  {
    Console.WriteLine("DBFile.Execute");
    return true;
  }
}

public class CSharpApp
{
  static void Main(string[] args)
  {
    var db = DBFile.Create("text.txt");
  }
}
