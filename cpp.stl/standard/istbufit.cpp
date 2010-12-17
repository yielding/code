#include<iostream>
#include<fstream>

int main ( )
{
  using namespace std;

  // open the file is_iter.out for reading and writing
  ofstream out("is_iter.out", ios_base::out | ios_base::in | ios_base::trunc);

  // output the example sentence into the file
  out << "Ceci est un simple example pour demontrer le" << endl;
  out << "fonctionement de istreambuf_iterator";

  // seek to the beginning of the file
  out.seekp(0);

  // construct an istreambuf_iterator pointing to
  // the ofstream object underlying streambuffer
  istreambuf_iterator<char,char_traits<char> > iter(out.rdbuf());

  // construct an end of stream iterator
  istreambuf_iterator<char,char_traits<char> > end_of_stream_iterator;

  cout << endl;

  // output the content of the file
  while (!iter.equal(end_of_stream_iterator))
  {
    // use both operator++ and operator*
    cout << *iter++;
  }

  cout << endl; 

  return 0;
}
