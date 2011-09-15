import java.util.*;

// key   = UID
// value = MenuItemk
class IDManager {

  private static int _newID = 50000;       // consult Win32 Menu ID value range 

  private static HashMap map = new HashMap();

  public static int newID(String mi) {
    Integer id = new Integer(_newID++); 
    if (map.put(id, mi) != null) {
      System.out.println("Error : same id exists");
    }
    return _newID-1;
  }

  public int getID(String mi) {

    if (map.containsValue(mi)) {
      Iterator i = map.values().iterator();
      while (i.hasNext()) {
        String value = (String)i.next();
        if (value == mi) return Integer.parseInt(value);
      }
    }

    return -1;
  }

  public String getResource(int id) {

    Iterator i = map.keySet().iterator();
    while (i.hasNext()) {
      Integer key = (Integer) i.next();
      if (key.intValue() == id) return (String)map.get(key);
    }

    return (String)null;
  }


  public static IDManager Instance() {
    return instance;
  }

  // for debug
  public int size() {
    return map.size();
  }

  private static IDManager instance = new IDManager();

  private IDManager() { }
}

public class HashMapTest {
  public static void main(String args[]) {
    String s1  = new String("11");
    String s2  = new String("22");
    String s3  = new String("33");
    String s4  = new String("4");
    String s5  = new String("5");
    String s6  = new String("6");
    String s7  = new String("7");
    String s8  = new String("8");
    String s9  = new String("9");
    String s10 = new String("10");
    IDManager.Instance().newID(s1);
    IDManager.Instance().newID(s2);
    IDManager.Instance().newID(s3);
    IDManager.Instance().newID(s4);
    IDManager.Instance().newID(s5);
    IDManager.Instance().newID(s6);
    IDManager.Instance().newID(s7);
    IDManager.Instance().newID(s8);
    IDManager.Instance().newID(s9);
    IDManager.Instance().newID(s10);

    for (int i=0; i<10; i++) 
      System.out.println(IDManager.Instance().getResource(50000 + i));

    /*
       HashMap m = new HashMap();
       Integer I = new Integer(10);
       String  S = new String("leech");

       m.put(I, S);
       System.out.println((String) m.get(I));
       */
  }
}
