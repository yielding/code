#include<iostream>
#include<fstream>

int main ( )
{
  using namespace std;

  // create a filebuf object
  filebuf buf;

  // open the file iter_out and link it to the filebuf object
  buf.open("iter_out", ios_base::in | ios_base::out | ios_base::trunc );

  // create an ostreambuf_iterator and link it to 
  // the filebuf object
  ostreambuf_iterator<char,char_traits<char> > out_iter(&buf);

  // output into the file using the ostreambuf_iterator
  for(char i=64; i<127; i++ )
    out_iter = i;

  // seek to the beginning of the file
  buf.pubseekpos(0);

  // create an istreambuf_iterator and link it to 
  // the filebuf object
  istreambuf_iterator<char,char_traits<char> > in_iter(&buf);

  // construct an end of stream iterator
  istreambuf_iterator<char,char_traits<char> > end_of_stream_iterator;

  cout << endl;

  // output the content of the file
  while( !in_iter.equal(end_of_stream_iterator) )

    // use both operator++ and operator*
    cout << *in_iter++;

  cout << endl;

  return 0;
}
