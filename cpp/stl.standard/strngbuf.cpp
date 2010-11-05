#include <iostream>
#include <sstream>
#include <string>

int main()
{
  using namespace std;

  // create a read/write string-stream object on tiny char
  // and attach it to an ostringstream object
  ostringstream out_1(ios_base::in | ios_base::out);

  // tie the istream object to the ostringstream object
  istream in_1(out_1.rdbuf());   

  // output to out_1
  out_1 << "Here is the first ouput";

  // create a string object on tiny char 
  string  string_ex("l'heure est grave !");

  // open a read only string-stream object on tiny char
  // and initialize it
  istringstream in_2(string_ex);

  // output in_1 to the standard output
  cout << in_1.rdbuf() << endl;

  // reposition in_1 at the beginning
  in_1.seekg(0);

  // output in_2 to the standard output
  cout << in_2.rdbuf() << endl;

  // reposition in_2 at the beginning
  in_2.seekg(0);

  stringbuf::pos_type pos;

  // get the current put position
  // equivalent to
  // out_1.tellp(); 
  pos = out_1.rdbuf()->pubseekoff(0,ios_base::cur,
                                  ios_base::out);

  // append the content of stringbuffer
  // pointed at by in_2 to the one 
  // pointed at by out_1
  out_1 << ' ' << in_2.rdbuf();

  // output in_1 to the standard output
  cout << in_1.rdbuf() << endl;

  // position the get sequence
  // equivalent to
  // in_1.seekg(pos);
  in_1.rdbuf()->pubseekpos(pos, ios_base::in);

  // output "l'heure est grave !"
  cout << in_1.rdbuf() << endl << endl;  

  return 0;
}
