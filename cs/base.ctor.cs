using System;
using System.IO;
using System.Text;

public abstract class SQLiteFile
{
  public SQLiteFile()
  {
    Console.WriteLine("SQLiteFile.ctor");
    Execute();
  }

  public abstract bool Execute();
}

public class DBFile: SQLiteFile
{
  public DBFile()
  {
    Console.WriteLine("DBFile.ctor");
  }

  public override bool Execute()
  {
    Console.WriteLine("DBFile.Execute");
    return true;
  }
}


public class CSharpApp
{
  static void Main(string[] args)
  {
    var db = new DBFile();
  }
}
