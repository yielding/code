import java.io.*;
import java.net.*;

public class KakaoClient {

    Socket socket = null;
    DataOutputStream out = null;
    DataInputStream  in  = null;

    public KakaoClient(String addr, int port) {
        try {
            this.socket = new Socket(addr, port);
            this.out = new DataOutputStream(new BufferedOutputStream(socket.getOutputStream()));
            this.in  = new DataInputStream(new BufferedInputStream(socket.getInputStream()));
        } catch(Exception e) {
        }
    }

    public String decryptString(String cipheredText, String salt) {
        try {
            byte[] cipheredBytes = cipheredText.getBytes("UTF-8");
            byte[]     saltBytes = salt.getBytes("UTF-8");

            out.writeByte(2);
            out.writeInt(cipheredBytes.length);
            out.write(cipheredBytes, 0, cipheredBytes.length);
            out.writeInt(saltBytes.length);
            out.write(saltBytes, 0, saltBytes.length);
            out.flush();

            byte packetType = in.readByte();
            if (packetType == 3) {
                int length = in.readInt();
                byte[] plainBytes = new byte[length];
                in.read(plainBytes, 0, length);
                return new String(plainBytes);
            }

            if (packetType == 4) {
                int length = in.readInt();
                byte[] plainBytes = new byte[length];
                in.read(plainBytes, 0, length);
                return new String(plainBytes);
            }

        } catch(IOException e) {
        }

        return null;
    }

    public String terminateServer() {
        try {
            out.writeByte(0);
            out.flush();
        } catch(Exception e) {
        }

        return null;
    }

    public String ping() {
        try {
            String body = "MDSBC";
            byte[] bodyBytes = body.getBytes("UTF-8");
            out.writeByte(1);
            out.writeInt(5);
            out.write(bodyBytes, 0, bodyBytes.length);
            out.flush();

            int reader = in.readByte();
            byte[] bodyBack = new byte[5];
            in.read(bodyBack, 0, 5);
            return new String(bodyBack);

        } catch(Exception e) {
        }

        return null;
    }

    public static void pl(String s) {
        System.out.println(s);
    }

    public static void main(String[] args) {

        String cipheredText = "m+oavcl6PVEo1RBcCFlKSQ==";
        String salt = "23303370";

        KakaoClient client = new KakaoClient("127.0.0.1", 7781);

        for (int i=0; i<10; i++)
        {
            System.out.print(i + " ");
            pl(client.decryptString(cipheredText, salt));
        }

        client.terminateServer();
        //pl(client.ping());

        try { Thread.sleep(1000); } catch(Exception e) { }
    }
}
