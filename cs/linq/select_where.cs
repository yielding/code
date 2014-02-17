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
  public static int Main(string[] args)
  {
    var s0 = new Student("leecH", 40 );
    var s1 = new Student("gunlee", 10 );

    var students = new List<Student>() { s0, s1 };
    var res = students.Where (i => i.Age == 50)
                      //.Select(i => i.Name)
                      .ToList();
    res.ForEach(n => Console.WriteLine(n));

    return 0;
  }
}
