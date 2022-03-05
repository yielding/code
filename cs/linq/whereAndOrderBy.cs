using System;
using System.IO;
using System.Linq;
using System.Collections.Generic;

class CSharpApp
{
  public class Student
  {
    public int    Age  { get; set; }
    public string Name { get; set; }
  }

  static void Main(string[] args)
  {
    var students = new List<Student>() {
      new Student() { Age=20, Name="leech" },
      new Student() { Age=20, Name="kamin" },
      new Student() { Age=21, Name="joo"   },
      new Student() { Age=21, Name="young" },
      new Student() { Age=21, Name="kyung" }
    };

    students.Where(e => e.Age > 20)
            .OrderBy(e => e.Name)
            .ForEach(e => Console.WriteLine(e.Name));
  }
}
