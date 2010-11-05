#include <iterator>
#include <vector>
#include <numeric>

#include <iostream>

int main ()
{
  using namespace std;

  vector<int> d;
  int total = 0;
  //
  // Collect values from cin until end of file
  // Note use of default constructor to get ending iterator
  //
  cout << "Enter a sequence of integers (eof to quit): " ;
  copy(istream_iterator<int,char,char_traits<char>,ptrdiff_t >(cin),
       istream_iterator<int,char,char_traits<char>,ptrdiff_t >(),
       inserter(d,d.begin()));
  //
  // stream the whole vector and the sum to cout
  //
  copy(d.begin(),d.end()-1, ostream_iterator<int>(cout," + "));

  if (d.size())
    cout << *(d.end()-1) << " = " <<  
      accumulate(d.begin(),d.end(),total) << endl;
  return 0;
}

