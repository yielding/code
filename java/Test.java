import java.io.*;
import java.net.*;

public class Test
{
  public static void pl(String s)
  {
    System.out.println(s);
  }

  public static void main(String[] args)
  {
    try
    {
      Socket sock = new Socket("192.168.10.127", 7781);

      DataOutputStream dos = new DataOutputStream(new BufferedOutputStream(sock.getOutputStream()));
      DataInputStream  dis = new DataInputStream(new BufferedInputStream(sock.getInputStream()));

      for (int i=0; i<1000000; i++)
      {
        String packet = "open:g:00001:1";
        dos.write(packet.getBytes());
        dos.flush();

        byte[] res = new byte[100];
        dis.read(res);

        pl(new String(res, 0, 100));
        Thread.sleep(1000);
      }
    }
    catch(IOException e)
    {
    }
    catch(InterruptedException e)
    {
    }
  }
}
