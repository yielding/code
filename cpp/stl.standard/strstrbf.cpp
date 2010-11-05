#include <iostream>
#include <strstream>
#include <iomanip>

int main()
{
  using namespace std;

  // create a read/write strstreambuf object
  // and attach it to an ostrstream object
  ostrstream out;

  // tie the istream object to the ostrstream object
  istream in(out.rdbuf());   

  // output to out_1
  out << "anticonstitutionellement is a big word !!!";

  // create a NTBS 
  char *p ="Le rat des villes et le rat des champs";

  // output the NTBS
  out << p << endl;   

  // resize the buffer
  if ( out.rdbuf()->pubsetbuf(0,5000) )
    cout << endl << "Success in allocating the buffer" << endl;

  // output the all buffer to stdout
  cout << in.rdbuf( );

  // output the decimal conversion of 100 in hex
  // with right padding and a width field of 200
  out << dec << setfill('!') << setw(200) << 0x100 << endl;  

  // output the content of the input sequence to stdout
  cout << in.rdbuf( ) << endl;

  // number of elements in the output sequence
  cout << out.rdbuf()->pcount() << endl;

  // resize the buffer to a minimum size
  if ( out.rdbuf()->pubsetbuf(0,out.rdbuf()->pcount()+1) )
    cout << endl << "Success in resizing the buffer" << endl;

  // output the content of the all array object
  cout << out.rdbuf()->str() << endl;

  return 0;
}
