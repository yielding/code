#include <string>
#include <set>

#include <iostream>
#include <fstream>

using namespace std;

void spellCheck (istream & dictionary, istream & text)
{
  typedef set <string> stringset;
  stringset words, misspellings;
  string word;
  istream_iterator<string,char,char_traits<char>, ptrdiff_t > eof, dstream(dictionary);
  //
  // First read the dictionary.
  //
  copy(dstream,eof,inserter(words,words.begin()));
  //
  // Next read the text.
  //
  while (text >> word)
    if (! words.count(word))
      misspellings.insert(word);
  //
  // Finally, output all misspellings.
  //
  cout << endl;
  cout << "Misspelled words:" << endl;
  copy (misspellings.begin(), misspellings.end(),
      ostream_iterator<string>(cout, "\n"));
}

int main ()
{
  cout << "Enter text:";
  ifstream words("words");
  spellCheck(words, cin);
  cout << "End of spell check program" << endl;
  return 0;
}
