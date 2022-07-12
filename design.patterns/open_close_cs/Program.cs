using System;
using System.Collections.Generic;

namespace OpenClosedPrinciple
{
    class Program
    {
        static void Main(string[] args)
        { 
            var all = new List<Product> {
                new Product("Apple",      Color.Green, Size.Small),
                new Product("Tree" ,      Color.Green, Size.Large),
                new Product("House",      Color.Blue,  Size.Large),
                new Product("Strawberry", Color.Red,   Size.Small)
            };

            example1(all);
            example2(all); // 복합 명세
            example3(all); // 연산자 오버로딩
        }

        private static void example3(List<Product> all)
        {
            var bf = new BetterFilter();

            var greenAndLarge = new ColorSpec(Color.Green) & new SizeSpec(Size.Large);
            var redOrLarge    = new ColorSpec(Color.Red)   | new SizeSpec(Size.Large);

            bf.Filter(all, greenAndLarge)
              .ForEach(gl => print(gl));

            bf.Filter(all, redOrLarge)
              .ForEach(rl => print(rl));
        }

        private static void example2(List<Product> all)
        {
            var bf = new BetterFilter();

            var greenAndLarge = new AndSpec<Product>(
                new SizeSpec(Size.Large),
                new ColorSpec(Color.Green));

            bf.Filter(all, greenAndLarge)
              .ForEach(bg => print(bg));
        }

        private static void example1(List<Product> all)
        {
            var bf = new BetterFilter();

            bf.Filter(all, new ColorSpec(Color.Green))
              .ForEach(g => print(g));
        }

        private static void print(Product s)
        {
            Console.WriteLine(s);
        }
    }
}
