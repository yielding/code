#include <iostream>
#include <fstream>

using namespace std;

int main ()
{
  // create a read/write file-stream object on tiny char
  // and attach it to the file "filebuf.out"
  ofstream out("filebuf.dat",ios_base::in | ios_base::out | ios_base::trunc);

  // tie the istream object to the ofstream object
  istream in(out.rdbuf());   

  // output to out
  out << "Il errait comme un ame en peine";

  // seek to the beginning of the file
  in.seekg(0);

  // output in to the standard output
  cout << in.rdbuf() << endl;

  // close the file "filebuf.out"
  out.close();

  // open the existing file "filebuf.out"
  // and truncate it
  out.open("filebuf.dat",ios_base::in |
      ios_base::out | ios_base::trunc);

  // set the buffer size
  out.rdbuf()->pubsetbuf(0,4096);

  // open the source code file
  ifstream ins("filebuf.cpp");

  //output it to filebuf.out
  out << ins.rdbuf();

  // seek to the beginning of the file
  out.seekp(0);

  // output the all file
  cout << out.rdbuf();  

  return 0;
}
