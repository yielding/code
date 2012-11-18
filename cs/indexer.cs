using System;

namespace Indexer 
{
    class MyList
    {
        public int[] array;

        public MyList()
        {
            array = new int[3];
        }

        public int this[int index]
        {
            get { return array[index]; }

            set 
            {
                if (index >= array.Length)
                {
                    Array.Resize<int>(ref array, index+1);
                    Console.WriteLine("Array Resized: {0}", array.Length);
                }

                array[index] = value;
            }
        }

        public int Length
        {
            get { return array.Length; }
        }
    }
}

namespace CSharpApp
{
    class MainApp
    {
        static void Main(string[] args)
        {
            var l = new Indexer.MyList();

            for (int i=0; i<5; i++) l[i] =i;

            for (int i=0; i<l.Length; i++)
                Console.WriteLine(l[i]);
        }
    }
}
