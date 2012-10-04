using System;

namespace Generic
{
    class MyList<T>
    {
        public T[] array;

        public MyList()
        {
            array = new T[3];
        }

        public T this[int index]
        {
            get
            {
                return array[index];
            }

            set 
            {
                if (index >= array.Length)
                {
                    Array.Resize<T>(ref array, index+1);
                    Console.WriteLine("Array Resized : {0}", array.Length);
                }

                array[index] = value;
            }
        }

        public int Length
        {
            get { return array.Length; }
        }
    }

    class CSharpApp
    {
        static void Main(string[] args)
        {
            var strs = new MyList<string>();
            strs[0]  = "abc";
            strs[1]  = "bbc";
            strs[2]  = "cbc";
            strs[3]  = "dbc";
            strs[4]  = "ebc";

            for (int i=0; i<strs.Length; i++)
                Console.WriteLine(strs[i]);

            var ints = new MyList<int>();
            ints[0]  = 1;
            ints[1]  = 2;
            ints[2]  = 3;
            ints[3]  = 4;
            ints[4]  = 5;
            ints[5]  = 6;

            for (int i=0; i<ints.Length; i++)
                Console.WriteLine(ints[i]);

        }
    }
}
