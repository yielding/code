import java.io.Serializable;
import java.io.FileOutputStream;
import java.io.ObjectOutputStream;
import java.io.IOException;

class Test2Serial implements Serializable {
    public byte version = 100;
    public int  size    = 0x1000;;
    public byte count   = 0;

    public static void main(String args[]) throws IOException {
        FileOutputStream fos = new FileOutputStream("temp2.out");
        ObjectOutputStream oos = new ObjectOutputStream(fos);
        TestSerial ts = new TestSerial();
        oos.writeObject(ts);
        oos.flush();
        oos.close();
    }
}

