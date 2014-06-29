import java.io.*;

public class LineNo {
  public static void main(String args[]) {
    if (args.length == 0) {
      System.err.println("Input filename is needed...");
      System.exit(1);
    }

    try {
      BufferedReader  in = new BufferedReader(new FileReader(args[0]));
      BufferedWriter out = new BufferedWriter(new FileWriter(args[0] + "_line"));

      int ln = 0;
      String s;
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
