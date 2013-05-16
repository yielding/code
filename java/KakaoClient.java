import java.io.*;
import java.net.*;

public class KakaoClient {
    public static void pl(String s) {
        System.out.println(s);
    }

    public static void main(String[] args) {
        String cipheredText = "m+oavcl6PVEo1RBcCFlKSQ==";
        String salt = "23303370";

        for (int i=0; i<10; i++)
        {
            try {
                byte[] cipheredBytes = cipheredText.getBytes("UTF-8");
                byte[]     saltBytes = salt.getBytes("UTF-8");

                //Socket socket = new Socket("172.16.17.1", 7781);
                Socket socket = new Socket("127.0.0.1", 7781);

                DataOutputStream out = new DataOutputStream(new BufferedOutputStream(socket.getOutputStream()));
                DataInputStream  in  = new DataInputStream(new BufferedInputStream(socket.getInputStream()));

                out.writeByte(2);
                out.flush();
                out.writeInt(cipheredBytes.length);
                out.write(cipheredBytes, 0, cipheredBytes.length);
                out.writeInt(saltBytes.length);
                out.write(saltBytes, 0, saltBytes.length);
                out.flush();

                byte packetType = in.readByte();
                if (packetType == 1) {
                    int length = in.readInt();
                    byte[] plainBytes = new byte[length];
                    in.read(plainBytes, 0, length);
                    String res = new String(plainBytes);
                    System.out.println(res);
                }
                Thread.sleep(1000);
            } catch(InterruptedException e) {
            } catch(IOException e) {
            }

        }
    }
}
