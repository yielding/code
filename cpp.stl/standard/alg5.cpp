// refer (1)
#include <vector>
#include <list>
#include <algorithm>
#include <numeric>
#include <string.h>
#include <string>

#include <iostream>

using namespace std;

//
// Forward declarations.
//
bool isVowel (char);
void count_example();
void accumulate_example();

template<class T>
list<T>& listadd(list<T>& base, T& newValue);

void inner_product_example();

void equal_example();

bool isVowel (char c)
{
  switch (c)
  {
    case 'a': case 'A':
    case 'e': case 'E':
    case 'i': case 'I':
    case 'o': case 'O':
    case 'u': case 'U':
      return true;
  }
  return false;
}

//
// Illustrate the use of the count function.
//
void count_example ()
{
  int ecount     = 0;
  int vowelCount = 0;

  char * text = "Now is the time to begin";

  count (text, text + strlen(text), 'e', ecount);
  count_if (text, text + strlen(text), isVowel, vowelCount);

  cout << "There are " << ecount << " letter e's " << endl << "and "
    << vowelCount << " vowels in the text:" << text << endl;
}

//
// Add n to 1 to list.
//
list<int>& intReplicate(list<int>& nums, int n)
{
  while (n) 
    nums.push_back(n--);

  return nums;
}

//
// Illustrate the use of the accumulate function.
//
void accumulate_example()
{
  int numbers[] = { 1, 2, 3, 4, 5 };

  int sum     = accumulate(numbers, numbers+5, 0);
  int product = accumulate(numbers, numbers+5, 1, multiplies<int>());

  cout << "The sum of the first five numbers is "     << sum     << endl;
  cout << "The product of the first five numbers is " << product << endl;
  //
  // Example with different types for init.
  //
  list<int> nums;
  nums = accumulate(numbers, numbers+5, nums, intReplicate);
  copy (nums.begin(), nums.end(), ostream_iterator<int,char,char_traits<char> >(cout, " "));
  cout << endl;
}

//
// Illustrate the use of the inner_product function.
//
void inner_product_example ()
{
  int a[] = { 4, 3, -2 };
  int b[] = { 7, 3, 2  };
  //
  // Example 1, simple inner product.
  //
  int in1 = inner_product(a, a+3, b, 0);
  cout << "Inner product is " << in1 << endl;
  //
  // Example 2, using different operations.
  //
  bool anyequal = inner_product(a, a+3, b, true, logical_or<bool>(),
      equal_to<int>());
  cout << "any equal? " << anyequal << endl;  
}

//
// Illustrate the use of the equal function.
//
void equal_example ()
{
  int a[] = { 4, 5, 3 };
  int b[] = { 4, 3, 3 };
  int c[] = { 4, 5, 3 };

  cout << "a = b is:" << equal(a, a+3, b) << endl;
  cout << "a = c is:" << equal(a, a+3, c) << endl;
  cout << "a pair-wise-greater_equal b is"
       << equal(a, a+3, b, greater_equal<int>()) << endl;
}

//
// Illustrate the use of the lexical_comparison function.
//

void lexical_comparison_example ()
{
  char* wordOne = "everything";
  char* wordTwo = "everybody";

  cout << "compare everybody to everything "
       << lexicographical_compare(wordTwo, wordTwo+strlen(wordTwo), wordOne,
          wordOne+strlen(wordOne))
       << endl;

  int a[] = { 3, 4, 5, 2 };
  int b[] = { 3, 4, 5 };
  int c[] = { 3, 5 };

  cout << "compare a to b: " << lexicographical_compare(a,a+4,b,b+3) << endl;
  cout << "compare a to c: " << lexicographical_compare(a,a+4,c,c+2) << endl;
}

int main ()
{
  cout << "STL generic algorithms -- algorithms that produce scalar results" << endl;

  count_example();
  accumulate_example();
  inner_product_example();
  equal_example();
  lexical_comparison_example();

  cout << "End of scalar algorithms test"  << endl;

  return 0;
}
