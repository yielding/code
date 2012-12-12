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

    public override bool Equals(object obj)
    {
        var rhs = obj as Coord;
        if (rhs == null)
            return false;

        return X == rhs.X && Y == rhs.Y;
    }

    public override int GetHashCode()
    {
        return X.GetHashCode() ^ Y.GetHashCode();
    }
}

public class CShaprApp
{
    public static void Main(string[] args)
    {
        HashSet<Coord> coords = new HashSet<Coord>();
        coords.Add(new Coord(1, 1));
        coords.Add(new Coord(1, 1));
        coords.Add(new Coord(1, 3));
        Console.WriteLine(coords.Count);
    }
}

