#include <algorithm>
#include <list>

#include <iostream>

int main ()
{
  using namespace std;

  //
  // Initialize a list sequence and subsequence with characters.
  //
  char seq[40]    = "Here's a string with a substring in it";
  char subseq[10] = "substring";
  list<char> sequence(seq, seq+38);
  list<char> subseqnc(subseq, subseq+9);
  //
  // Print out the original sequence.
  //
  cout << endl << "The subsequence, " << subseq << ", was found at the ";
  cout << endl << "location identified by a '*'" << endl << "     ";
  //
  // Create an iterator to identify the location of 
  // subsequence within sequence.
  //
  list<char>::iterator place;
  //
  // Do search.
  //
  place = search(sequence.begin(), sequence.end(),
      subseqnc.begin(), subseqnc.end());
  //
  // Identify result by marking first character with a '*'.
  //
  *place = '*';
  //
  // Output sequence to display result.
  //
  for (list<char>::iterator i = sequence.begin(); i != sequence.end(); i++)
    cout << *i;

  cout << endl;

  return 0;
}
