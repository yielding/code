import java.io.*;

public class LineNo {
  public static void main(String args[]) {
    if (args.length == 0) {
      System.err.println("Input filename is needed...");
      System.exit(1);
    }

    try {
      var  in = new BufferedReader(new FileReader(args[0]));
      var out = new BufferedWriter(new FileWriter(args[0] + "_line"));

      var ln = 0;
      var s = "";
      while ((s = in.readLine()) != null) {
        String res = String.format("%-5d : %s", ln++, s);
        System.out.println(res);
        out.write(res+"\n");
      }
      out.close();
    } catch(IOException e) {
      System.err.println(e);
    }
  }
}
