#include<iostream>
#include<sstream>
#include<string>
#include<iomanip>

int main()
{
  using namespace std;

  long   l= 20;

#ifndef _RWSTD_NO_WIDE_CHAR
  wchar_t *ntbs=L"Il avait l'air heureux";
  wchar_t c;
  wchar_t buf[50];

  // create a read/write string-stream object on wide char
  // and attach it to an wistringstream object
  wistringstream in(ios_base::in | ios_base::out);

  // tie the ostream object to the wistringstream object
  wostream out(in.rdbuf());   

  // output ntbs in out
  out << ntbs;

  // output each word on a separate line
  while ( in.get(c) )
  {
    if ( wistringstream::traits_type::eq(c,L' ') ) 
      wcout << endl;
    else
      wcout << c;
  }
  wcout << endl << endl;

  // move back the input sequence to the beginning
  in.seekg(0); 

  // clear the state flags
  in.clear();

  // does the same thing as the previous code
  // output each word on a separate line
  while ( in >> buf )
    wcout << buf << endl; 

  wcout << endl << endl;
#endif

  // create a tiny string object
  string test_string("Il dormait pour l'eternite");

  // create a read/write string-stream object on char
  // and attach it to an istringstream object
  istringstream in_bis(ios_base:: in | ios_base::out |
      ios_base::app );

  // create an ostream object
  ostream out_bis(in_bis.rdbuf());  

  // initialize the string-buffer with test_string
  in_bis.str(test_string);

  out_bis << endl;

  // output the base info before each integer
  out_bis << showbase;

  ostream::pos_type pos= out_bis.tellp();

  // output l in hex with a field with of 20 
  out_bis << hex << setw(20) << l << endl;

  // output l in oct with a field with of 20
  out_bis << oct << setw(20) << l << endl;

  // output l in dec with a field with of 20
  out_bis << dec << setw(20) << l << endl;

  // output the all buffer
  cout << in_bis.rdbuf();

  // seek the input sequence to pos  
  in_bis.seekg(pos);

  int a,b,d;

  in_bis.unsetf(ios_base::basefield);

  // read the previous outputted integer
  in_bis >> a >> b >> d;

  // output 3 times 20
  cout << a << endl << b << endl << d << endl;

  return 0;
}
