using System;
using System.Diagnostics;
using System.Net;
using System.Net.Sockets;
using System.Text;

namespace Echo
{
    class Application
    {
        public static int Main(string[] args)
        {
            if (args.Length < 4)
            {
                Console.WriteLine(
                  "사용법: {0} <bind ip> <bind port> <server ip> <message>",
                  Process.GetCurrentProcess().ProcessName);
                return -1;
            }

            Console.WriteLine("begin..");
            var bindIp   = args[0];
            var bindPort = Convert.ToInt32(args[1]);
            var serverIp = args[2];
            var serverPort = 5425;
            var message    = args[3];

            try 
            {
                var clientAddr = new IPEndPoint(IPAddress.Parse(bindIp), bindPort);
                var serverAddr = new IPEndPoint(IPAddress.Parse(serverIp), serverPort);
                Console.WriteLine(
                       "클라이언트: {0}, 서버{1}",
                        clientAddr.ToString(), serverAddr.ToString());

                var client = new TcpClient(clientAddr);
                client.Connect(serverAddr);

                var codec  = System.Text.Encoding.Default;
                var data   = codec.GetBytes(message);
                var stream = client.GetStream();
                stream.Write(data, 0, data.Length);

                Console.WriteLine("송신: {0}", message);

                data = new byte[256];
                int bytes = stream.Read(data, 0, data.Length);
                var responseData = codec.GetString(data, 0, bytes);
                Console.WriteLine("수신: {0}", responseData);

                stream.Close();
                client.Close();
            }
            catch(SocketException e)
            {
                Console.WriteLine(e);
            }

            Console.WriteLine("클라이언트를 종료합니다");

            return 0;
        }
    }
}
