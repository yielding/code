import flash.XMLSocket;

class ClientFlash
{
  public static function main():Void
  {
    var socket:XMLSocket = new XMLSocket();
    socket.onConnect = function(success:Bool):Void
    {
      if (success)
        trace("Successfully connected");
      else
        trace("COnnection could not be established");
    }

    socket.onData = function(src:String):Void
    {
      trace("Received : |" + src + "|");
    }

    socket.onClose = function(): Void
    {
      trace("Connection closed");
    }
    trace ("Connecting ...");
    socket.connect("localhost", 10240);
  }
}
