import java.util.*;

class Mandelbrot
{  
  static int BAILOUT = 16;
  static int MAX_ITERATIONS = 1000;

  private static int iterate(float x, float y)
  {
    float cr = y-0.5f;
    float ci = x;
    float zi = 0.0f;
    float zr = 0.0f;
    int i = 0;
    while (true) {
      i++;
      float temp = zr * zi;
      float zr2 = zr * zr;
      float zi2 = zi * zi;
      zr = zr2 - zi2 + cr;
      zi = temp + temp + ci;
      if (zi2 + zr2 > BAILOUT)
        return i;
      if (i > MAX_ITERATIONS)
        return 0;
    }
  }

  public static void main(String args[])
  {
    Date d1 = new Date();

    int x,y;
    for (y = -39; y <= 39; y++) {
      System.out.print("\n");
      for (x = -39; x <= 39; x++) {
        if (iterate(x/40.0f,y/40.0f) == 0) 
          System.out.print("*");
        else
          System.out.print(" ");

      }
    }

    Date d2 = new Date();
    long diff = d2.getTime() - d1.getTime();
    System.out.println("\nJava Elapsed: " + diff/1000.0f);
  }
}

