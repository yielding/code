#include<iostream>
#include<istream>
#include<fstream>

int main()
{
  using namespace std;

  float f= 3.14159;
  int   i= 3;
  char  s[200];

  // open a file for read and write operations 
  ofstream out("example", ios_base::in | ios_base::out
      | ios_base::trunc);

  // tie the istream object to the ofstream filebuf 
  istream  in (out.rdbuf());

  // output to the file
  out << "He lifted his head and pondered." << endl;
  out << f << endl;
  out << i << endl;

  // seek to the beginning of the file
  in.seekg(0);

  f = i = 0;

  // read from the file using formatted functions
  in >> s >> f >> i;

  // seek to the beginning of the file
  in.seekg(0,ios_base::beg);

  // output the all file to the standard output
  cout << in.rdbuf();

  // seek to the beginning of the file
  in.seekg(0);

  // read the first line in the file
  // "He lifted his head and pondered."
  in.getline(s,100);

  cout << s << endl;

  // read the second line in the file
  // 3.14159
  in.getline(s,100);

  cout << s << endl;

  // seek to the beginning of the file
  in.seekg(0);

  // read the first line in the file
  // "He lifted his head and pondered."
  in.get(s,100);

  // remove the newline character
  in.ignore();

  cout << s << endl;

  // read the second line in the file
  // 3.14159
  in.get(s,100);

  cout << s << endl;

  // remove the newline character
  in.ignore();

  // store the current file position   
  istream::pos_type position = in.tellg();

  out << "replace the int" << endl;

  // move back to the previous saved position
  in.seekg(position);   

  // output the remain of the file
  // "replace the int"
  // this is equivalent of
  // cout << in.rdbuf();
  while( !ifstream::traits_type::eq_int_type( in.peek(),
        ifstream::traits_type::eof()) )
    cout << ifstream::traits_type::to_char_type(in.get()); 

  cout << "\n\n\n" << flush;

  return 0;
}
