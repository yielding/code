#include <vector>
#include <list>
#include <algorithm>
#include <functional>
#include <ctype.h>
#include <string>
#include <string.h>

#include <iostream>

using namespace std;

class iotaGenerator
{
public:
  iotaGenerator (int iv) : current(iv) { }
  int operator () () { return current++; }
private:
  int current;
};

bool isEven (int n) { return 0 == (n % 2); }


ostream_iterator<int,char,char_traits<char> > out_(cout, " ");

void reverse_example ()
{
  cout << "Illustrate reverse algorithm" << endl;
  //
  // Example 1, reversing a string.
  //
  char ctext[30] = "Rats live on no evil star";
  char text[30];
  strcpy(text,ctext);
  reverse (text, text + strlen(text));
  cout << text << endl;
  //
  // Example 2, reversing a list.
  //
  list<int> iList;
  generate_n(inserter(iList, iList.begin()), 10, iotaGenerator(2));
  reverse (iList.begin(), iList.end());
  copy (iList.begin(), iList.end(), out_);
  cout << endl;    
}

//
// Illustrate the use of the replace function.
//

void replace_example ()
{
  cout << "Illustrate replace algorithm" << endl;
  //
  // Make vector 0 1 2 3 4.
  //
  vector<int> numbers(11);    
  for (int i = 0; i < 11; i++)
    numbers[i] = i < 5 ? i : 10 - i;
  copy (numbers.begin(), numbers.end(), out_);
  cout << endl;
  //
  // Replace 0 by 2.
  //
  replace (numbers.begin(), numbers.end(), 3, 7);
  copy (numbers.begin(), numbers.end(), out_);
  cout << endl;
  //
  // Replace even numbers by 9.
  //
  replace_if (numbers.begin(), numbers.end(), isEven, 9);
  copy (numbers.begin(), numbers.end(), out_);
  cout << endl;
  //    
  // Copy into a list, replacing 9 by 3.
  //
  int aList[] = { 2, 1, 4, 3, 2, 5 };
  int bList[6];
  int cList[6];
  replace_copy (aList, aList+6, &bList[0], 2, 7);
  replace_copy_if (bList, bList+6, &cList[0], bind2nd(greater<int>(), 3), 8);
  copy (bList, bList + 6, out_); cout << endl;
  copy (cList, cList + 6, out_); cout << endl;
}

//
// Illustrate the use of the rotate function.
//

void rotate_example ()
{
  cout << "Illustrate rotate algorithm" << endl;
  //
  // Create the list 1 2 3 ... 10
  //
  list<int> iList;
  generate_n(inserter(iList, iList.begin()), 10, iotaGenerator(1));
  //
  // Find the location of the seven.
  //
  list<int>::iterator middle = find(iList.begin(), iList.end(), 7);
  //
  // Now rotate around that location.
  //
  rotate(iList.begin(), middle, iList.end());
  copy (iList.begin(), iList.end(), out_);
  cout << endl;
  //    
  // Rotate again around the same location.
  //
  list<int> cList;
  rotate_copy(iList.begin(), middle,iList.end(),
      inserter(cList, cList.begin()));
  copy (cList.begin(), cList.end(), out_);
  cout << endl; 
}

//
// Illustrate the use of the paration function.
//

void partition_example ()
{
  cout << "Illustrate partition algorithm" << endl;
  //
  // First make the vector 1 2 3 ... 10.
  //
  vector<int> numbers(10);
  generate(numbers.begin(), numbers.end(), iotaGenerator(1));
  //
  // Now put the odd values low, even values high.
  //
  vector<int>::iterator result = partition(numbers.begin(),numbers.end(),
      isEven);
  copy (numbers.begin(), numbers.end(), out_);
  cout << endl;
  cout << "middle location " << result - numbers.begin() << endl; 
  //
  // Now do a stable partition.
  //
  generate(numbers.begin(), numbers.end(), iotaGenerator(1));
  copy (numbers.begin(), numbers.end(), out_);
  cout << endl;
}

bool nameCompare (char * a, char * b) { return strcmp(a, b) <= 0; }

//
// Illustrate the use of the next_permutation function.
//

void permutation_example ()
{
  //
  // Start with the values 1 2 3 in sequence.
  //
  int start [] = {1, 2, 3 };

  do
  {
    copy (start, start + 3, out_);
    cout << endl;
  }
  while (next_permutation(start, start + 3));

  char * names[] = { "Alpha", "Beta", "Gamma" };

  do
  {
    copy (names, names + 3, out_);
    cout << endl;
  }
  while (next_permutation(names, names + 3, nameCompare));

  char * cword = "bela";
  char word[4];
  strcpy(word,cword);

  do
    cout << word << ' ';
  while (prev_permutation(word, &word[4]));

  cout << endl;   
}

//
// Illustrate the use of the inplace_merge function.
//

void inplace_merge_example ()
{
  cout << "Illustrate inplace merge algorithm" << endl;
  //
  // First generate the numbers 0 2 4 6 8 1 3 5 7 9.
  //
  vector<int> numbers(10);
  for (int i = 0; i < 10; i++)
    numbers[i] = i < 5 ? 2 * i : 2 * (i - 5) + 1;

  copy (numbers.begin(), numbers.end(), out_);
  cout << endl; 
  vector<int>::iterator midvec = find(numbers.begin(), numbers.end(), 1);
  //
  // Copy them into a list.
  //
  list<int> numList;
  copy(numbers.begin(), numbers.end(), inserter(numList, numList.begin()));
  list<int>::iterator midList = find(numList.begin(), numList.end(), 1);
  copy (numList.begin(), numList.end(), out_);
  cout << endl; 
  //
  // Now put them back together.
  //
  inplace_merge(numbers.begin(), midvec, numbers.end());
  inplace_merge(numList.begin(), midList, numList.end());
  copy (numList.begin(), numList.end(), out_);
  cout << endl; 
}

struct RandomInteger
{
  int operator() (int m) { return rand() % m; }
};

//
// Illustrate the use of the random_shuffle function.
//

void random_shuffle_example ()
{
  //
  // First make the vector 1 2 3 ... 10.
  //
  vector<int> numbers(10);
  generate(numbers.begin(), numbers.end(), iotaGenerator(1));

  RandomInteger random;

  //
  // Randomly shuffle the elements.
  //
  random_shuffle(numbers.begin(), numbers.end(), random);
  copy (numbers.begin(), numbers.end(), out_);
  cout << endl;


  //
  // Do it again.
  //
  random_shuffle(numbers.begin(), numbers.end(), random);
  copy (numbers.begin(), numbers.end(), out_);
  cout << endl;
}

int main ()
{
  cout << "STL generic algorithms -- in-place algorithms" << endl;

  reverse_example();
  replace_example();
  rotate_example();
  partition_example();
  permutation_example();
  inplace_merge_example();
  random_shuffle_example();

  cout << "End of in-place algorithm sample program"  << endl;

  return 0;
}
