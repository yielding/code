using System;
using System.Text;
using System.Security.Cryptography;

namespace HashTest
{
  class Program
  {
    public static string BytesToStr(byte[] bytes)
    {
      var str = new StringBuilder();
      for (int i = 0; i < bytes.Length; i++)
        str.AppendFormat("{0:X2}", bytes[i]);

      return str.ToString();
    }

    public static void Test1(byte[] data)
    {
      var sha1 = new SHA1CryptoServiceProvider();
      for (int x=0; x<10; x++)
      { 
        sha1.Initialize();
        for (int i=0; i<10; i++)
          sha1.TransformBlock(data, i*10, 10, data, i*10);

        sha1.TransformFinalBlock(data, 100, 0);
        Console.WriteLine(BytesToStr(sha1.Hash));
      }
    }

    public static void Test2(byte[] data)
    {
      var sha1 = new SHA1CryptoServiceProvider();
      for (int x=0; x<10; x++)
        Console.WriteLine(BytesToStr(sha1.ComputeHash(data, 0, data.Length)));
    }

    static void Main(string[] args)
    {
      var data = new byte[100];
      for (int i=0; i<100; i++)
        data[i] = (byte)i;

      Test1(data);
      Test2(data);
    }
  }
}
