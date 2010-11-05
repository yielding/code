import java.net.*;
import java.io.*;
import java.lang.*;
import java.text.*;

public class ServerTest 
{
  public static void main(String arg[]) 
  {
    ServerSocket srv = null;
    try 
    {
      int port = 1010;
      srv = new ServerSocket(port);

      while(true) 
      {
        // Wait for connection from client.
        Socket socket = srv.accept();
        byte buf [] = new byte[100];
        DataInputStream  in  = null;
        DataOutputStream out = null;

        in  = new DataInputStream (new BufferedInputStream (socket.getInputStream()));
        out = new DataOutputStream(new BufferedOutputStream(socket.getOutputStream()));

        for (int i=0; i<4; i++) 
        {
          buf[i] = in.readByte();
          System.out.print(buf[i]);
          System.out.print(" ");
        }
        out.write(buf, 0, 100);
        socket.close();
        System.out.println("");
      }
    } 
    catch(IOException e) 
    {}
  }
}
