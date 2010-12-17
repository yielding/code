#include<iostream>
#include<sstream>

int main ( )
{
  using namespace std;

  float f= 3.14159;

  char* s= "Kenavo !";

  // create a read/write stringbuf object on char
  // and attach it to an istringstream object
  istringstream in( ios_base::in | ios_base::out );

  // tie the wostream object to the istringstream object
  ostream out(in.rdbuf());

  out << "test beginning !" << endl;

  // output f in scientific format
  out << scientific << f <<endl; 

  // store the current put-pointer position  
  ostream::pos_type pos = out.tellp();

  // output s 
  out << s << endl;   

  // output the all buffer to standard output
  cout << in.rdbuf() << endl;

  // position the get-pointer
  in.seekg(pos);

  // output s
  cout << in.rdbuf() << endl; 

  return 0;
}
