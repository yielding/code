class Client
{
  static function main()
  {
    var s = new neko.net.Socket();
    s.connect(new neko.net.Host("localhost"), 10240);
    while(true)
    {
      var l = s.input.readLine();
      trace(l);
      if (l == "exit")
      {
        s.close();
        break;
      }
    }
  }
}
