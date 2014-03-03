using System;
using System.IO;
using System.Linq;
using System.Collections.Generic;

public class Student
{
  public Student(string name, int age)
  {
    this.Name = name;
    this.Age = age;
  }

  public override string ToString()
  {
    return String.Format("{0}, {1}", this.Name, this.Age);
  }

  public string Name { get; set; }
  public int    Age  { get; set; }
}

class Application
{
  public static void TestList()
  {
    var s0 = new Student("leech", 40);
    var s1 = new Student("gunhee", 10);

    var students = new List<Student>() { s0, s1 };
    var res = students.Where (i => i.Age == 50)
                      //.Select(i => i.Name)
                      .ToList();
    res.ForEach(n => Console.WriteLine(n));
  }

  public static void TestMap()
  {
    var s0 = new Student("leech", 40);
    var s1 = new Student("gunhee", 10);
    var students = new Dictionary<string, Student>();

    students["leech"] = s0;
    students["gunhee"] = s1;

    try
    {
      var res = students.First(s => s.Key == null);
    }
    catch(Exception e)
    {
      Console.WriteLine("Couldn't find");
    }
  }

  public static int Main(string[] args)
  {
    TestMap();

    return 0;
  }
}
