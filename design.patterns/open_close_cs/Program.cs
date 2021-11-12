using System;
using System.Collections.Generic;

namespace OpenClosedPrinciple
{
    class Program
    {
        static void Main(string[] args)
        {
            var all = new List<Product> { 
                new Product("Apple", Color.Green, Size.Small),
                new Product("Tree", Color.Green, Size.Large),
                new Product("House", Color.Blue, Size.Large),
                new Product("Strawberry", Color.Red, Size.Small)
            };

            example1(all);
            example2(all); // 복합 명세
            example3(all); // 연산자 오버로딩
        }

        private static void example3(List<Product> all)
        {
            var bf = new BetterFilter();

            var greenAndLarge = new ColorSpec(Color.Green) & new SizeSpec(Size.Large);
            var redOrLarge = new ColorSpec(Color.Red) | new SizeSpec(Size.Large);

            var bigGreenThings = bf.filter(all, greenAndLarge);
            var bigRedThings = bf.filter(all, redOrLarge);

            Console.WriteLine("big green things");
            foreach (var bigGreenThing in bigGreenThings)
                Console.WriteLine(bigGreenThing.name + " / " + bigGreenThing.color + " / " + bigGreenThing.size);

            Console.WriteLine();
            Console.WriteLine("red or big things");
            foreach (var bigRedThing in bigRedThings)
                Console.WriteLine(bigRedThing.name + " / " + bigRedThing.color + " / " + bigRedThing.size);
        }

        private static void example2(List<Product> all)
        {
            var bf = new BetterFilter();

            var large = new SizeSpec(Size.Large);
            var green = new ColorSpec(Color.Green);
            var greenAndLarge = new AndSpec<Product>(large, green);

            var bigGreenThings = bf.filter(all, greenAndLarge);

            Console.WriteLine("big green things");
            foreach (var bigGreenThing in bigGreenThings)
                Console.WriteLine(bigGreenThing.name + " / " + bigGreenThing.color + " / " + bigGreenThing.size);

            Console.WriteLine();
        }

        private static void example1(List<Product> all)
        {
            var bf = new BetterFilter();

            var green = new ColorSpec(Color.Green);

            var greenThings = bf.filter(all, green);

            Console.WriteLine("green things");
            foreach (var greenThing in greenThings)
                Console.WriteLine(greenThing.name + " / " + greenThing.color);

            Console.WriteLine();
        }
    }
}
