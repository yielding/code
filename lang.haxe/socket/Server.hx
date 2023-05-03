class Server
{
  public static function main()
  {
    var s = new neko.net.Socket();
    s.bind(new neko.net.Host("127.0.0.1"), 10240);
    s.listen(1);
    trace("Starting server...");
    while (true)
    {
      var c:neko.net.Socket = s.accept();
      c.write("hello\n");
      c.write("your IP is " + c.peer().host + "\n");
      c.write("exit\n");
      c.shutdown(true, true);
      c.close();
      trace("Connection closed ");
    }
  }
}
