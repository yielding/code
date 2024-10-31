class MyObject {
  private String name;
  private String type;

  public MyObject(String name, String type) {
    this.name = name;
    this.type = type;
  }

  public String toString() {
    return name + " : " + type;
  }
}

class Fluently {
  public static Builder create() {
    return new Builder();
  }

  public static class Builder {
    private String name;
    private String type;

    public Builder withName(String name) {
      this.name = name;
      return this;
    }

    public Builder withType(String type) {
      this.type = type;
      return this;
    }

    public MyObject build() {
      return new MyObject(name, type);
    }
  }
}

class Program {
  public static void main(String[] args) {
    var object = Fluently.create()
                         .withName("Example")
                         .withType("Type A")
                         .build();

    System.out.println(object.toString());
  }
}