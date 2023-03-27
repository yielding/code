using System;
using System.Diagnostics;
using System.Net;
using System.Net.Sockets;
using System.Text;
using System.IO;
using System.Linq;
using System.Collections.Generic;

namespace Echo
{
  class Server
  {
    public static int Main(string[] args)
    {
      if (args.Length < 1)
      {
        Console.WriteLine("Usage: {0} <Bind IP>)", Process.GetCurrentProcess().ProcessName);
        return -1;
      }

      string bindIp = args[0];
      const int port = 5425;
      TcpListener server = null;
      try 
      {
        var localAddr = new IPEndPoint(IPAddress.Parse(bindIp), port);
        server = new TcpListener(localAddr);
        server.Start();

        Console.WriteLine("Echo server start");

        while (true)
        {
          TcpClient client = server.AcceptTcpClient();
          Console.WriteLine("client connected: {0}", 
              ((IPEndPoint)client.Client.RemoteEndPoint).ToString());

          var stream = client.GetStream();

          int length;
          string data = null;
          byte[] bytes = new byte[256];

          while ((length = stream.Read(bytes, 0, bytes.Length)) != 0)
          {
            data = Encoding.Default.GetString(bytes, 0, length);
            Console.WriteLine(String.Format("수신: {0}", data));

            var msg = Encoding.Default.GetBytes(data);
            stream.Write(msg, 0, msg.Length);
            Console.WriteLine(String.Format("송신: {0}", data));
          }

          stream.Close();
          client.Close();
        }
      }
      catch(SocketException e)
      {
        Console.WriteLine(e);
      }
      finally
      {
        server.Stop();
      }

      Console.WriteLine("서버를 종료합니다");

      return 0;
    }
  }
}
