using System;
using System.Collections.Generic;
using System.Text;
using System.IO;
using System.Diagnostics;
using System.Collections;

namespace euler {
    class Problem22 {
        public static void Main(string[] args)
        {
            new Problem22().APISort();
            new Problem22().BubbleSort();
            new Problem22().MergeSort();
        }

        public void APISort()
        {
            Stopwatch clock = Stopwatch.StartNew();

            var fname = Directory.GetCurrentDirectory()  + "/names.txt";
            var names = readInput(fname);

            int sumResult = 0;
            Array.Sort(names, new mySorter());

            for (int i=0; i<names.Length; i++) 
                sumResult += (i + 1) *sum(names[i]);

            clock.Stop();
            Console.WriteLine("The sum of all names are: {0}", sumResult);
            Console.WriteLine("Solution took {0} ms", clock.ElapsedMilliseconds);
        }

        public void BubbleSort()
        {
            Stopwatch clock = Stopwatch.StartNew();
            var fname = Directory.GetCurrentDirectory()  + "/names.txt";
            var names = readInput(fname);

            int sumResult = 0;
            names = BubbleSort(names);

            for (int i=0; i<names.Length; i++) 
                sumResult += (i + 1) *sum(names[i]);

            clock.Stop();
            Console.WriteLine("The sum of all names are: {0}", sumResult);
            Console.WriteLine("Solution took {0} ms", clock.ElapsedMilliseconds);
        }

        public void MergeSort()
        {
            Stopwatch clock = Stopwatch.StartNew();
            string   fname = Directory.GetCurrentDirectory()  + "/names.txt";
            string[] names = readInput(fname);

            int sumResult = 0;
            names = MergeSort(names);

            for (int i=0; i<names.Length; i++) 
                sumResult += (i + 1) *sum(names[i]);

            clock.Stop();
            Console.WriteLine("The sum of all names are: {0}", sumResult);
            Console.WriteLine("Solution took {0} ms", clock.ElapsedMilliseconds);
        }

        private string[] readInput(string filename) {
            StreamReader r = new StreamReader(filename);
            string line = r.ReadToEnd();
            r.Close();

            string[] names = line.Split(',');
            for (int i=0; i<names.Length; i++) 
                names[i] = names[i].Trim('"');

            return names;
        }

        private int sum(string name) {
            int result = 0;
            for (int i=0; i<name.Length; i++) 
                result += Convert.ToInt32(name[i])  - 64;

            return result;
        }

        private string[] MergeSort(string[] strings) {
            if (strings.Length <= 1) {
                return strings;
            }

            int firstPart = strings.Length / 2;            
            string[] strings1 = new string[firstPart];
            string[] strings2 = new string[strings.Length - firstPart];

            string[] sorted = new string[strings.Length];                                           

            // Split the array into two;
            for (int i = 0; i < firstPart; i++) {
                strings1[i] = strings[i];
            }

            for (int i = firstPart; i < strings.Length; i++) {
                strings2[i - firstPart] = strings[i];                
            }

            strings1 = MergeSort(strings1);
            strings2 = MergeSort(strings2);


            int j = 0;
            int k = 0;

            for (int i = 0; i < sorted.Length; i++) {
                if (j == strings1.Length) {
                    sorted[i] = strings2[k];
                    k++;
                } else if (k == strings2.Length) {
                    sorted[i] = strings1[j];
                    j++;           
                } else if (CompareStrings(strings1[j], strings2[k])) {
                    sorted[i] = strings1[j];
                    j++;
                } else {
                    sorted[i] = strings2[k];
                    k++;
                }
            }

            return sorted;
        }

        private string[] BubbleSort(string[] strings) {
            bool changed = true;

            while (changed) {
                changed = false;
                for (int i=1; i<strings.Length; i++) {
                    if (!CompareStrings(strings[i - 1], strings[i])) {
                        string temp = strings[i - 1];
                        strings[i - 1] = strings[i];
                        strings[i] = temp;
                        changed = true;
                    }
                }
            }

            return strings;
        }

        private bool CompareStrings(string s1, string s2) {
            int i = 0;

            while (i < s1.Length && i < s2.Length) {
                if (s1[i] == s2[i])  
                    i++;
                else
                    return s1[i] < s2[i];
            }

            return s1.Length < s2.Length;
        }

        public class mySorter: IComparer {
            int IComparer.Compare(object x, object y) {
                var str1 = x.ToString();
                var str2 = y.ToString();

                int i = 0;
                while (i < str1.Length && i < str2.Length) {
                    if (str1[i] == str2[i]) {
                        i++;
                    } else {
                        if (str1[i] < str2[i]) {
                            return -1;
                        } else {
                            return 1;
                        }                                                
                    }
                }

                return str1.Length - str2.Length;     
            }
        }
    }
}
