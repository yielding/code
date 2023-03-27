using System;
using System.Linq;
using System.Collections.Generic;

public class Coord
{
  public int X { get; set; }
  public int Y { get; set; }

  public Coord(int x, int y)
  {
    X = x;
    Y = y;
  }
}

public class CoordComparer : IEqualityComparer<Coord>
{
  public bool Equals(Coord x, Coord y)
  {
    return x.X == y.X && y.X == y.Y;
  }

  public int GetHashCode(Coord obj)
  {
    return obj.X.GetHashCode() + obj.Y.GetHashCode();
  }
}

public class CShaprApp
{
  public static void Main(string[] args)
  {
    HashSet<Coord> coords = new HashSet<Coord>(new CoordComparer());
    coords.Add(new Coord(1, 1));
    coords.Add(new Coord(1, 1));
    coords.Add(new Coord(1, 3));
    Console.WriteLine(coords.Count);
  }
}

