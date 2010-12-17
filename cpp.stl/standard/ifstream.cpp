#include<iostream>
#include<fstream>
#include<iomanip>

int main ( )
{
  using namespace std;

  long   l= 20;
  const char   *ntbs="Le minot passait la piece a frotter";
  char   c;
  char   buf[50];

  try {

    // create a read/write file-stream object on char
    // and attach it to an ifstream object
    ifstream in("ifstream.results.out",ios_base::in | ios_base::out |
        ios_base::trunc);

    if ( !in.is_open() ) 
      throw(ios_base::failure("Open error"));

    // tie the ostream object to the ifstream object
    ostream out(in.rdbuf());   

    // output ntbs in out
    out << ntbs << endl;

    // seek to the beginning of the file
    in.seekg(0);

    // output each word on a separate line
    while ( in.get(c) )
    {
      if ( ifstream::traits_type::eq(c,' ') ) 
        cout << endl;
      else
        cout << c;
    }
    cout << endl << endl;

    // move back to the beginning of the file
    in.seekg(0); 

    // clear the state flags
    in.clear();

    // does the same thing as the previous code
    // output each word on a separate line
    while ( in >> buf )
      cout << buf << endl; 

    cout << endl << endl;

    // output the base info before each integer
    out << showbase;

    ostream::pos_type pos= out.tellp();

    // output l in hex with a field with of 20 
    out << hex << setw(20) << l << endl;

    // output l in oct with a field with of 20
    out << oct << setw(20) << l << endl;

    // output l in dec with a field with of 20
    out << dec << setw(20) << l << endl;

    // move back to the beginning of the file
    in.seekg(0);

    // output the all file
    cout << in.rdbuf();

    // clear the flags 
    in.clear(); 

    // seek the input sequence to pos  
    in.seekg(pos);

    int a,b,d;

    in.unsetf(ios_base::basefield);

    // read the previous outputted integer
    in >> a >> b >> d;

    // output 3 times 20
    cout << a << endl << b << endl << d << endl;

  }
  catch(ios_base::failure var)
  {
    cout << var.what();
  }

  return 0;
}
