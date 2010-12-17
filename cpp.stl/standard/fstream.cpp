#include<iostream>
#include<fstream>

int main ( )
{
  using namespace std;

  // create a bi-directional fstream object 
  fstream inout("fstream.out",ios_base::in | ios_base::out | ios_base::trunc);

  // output characters
  inout << "Das ist die rede von einem man" << endl;
  inout << "C'est l'histoire d'un home" << endl;
  inout << "This is the story of a man" << endl;

  char p[100];

  // seek back to the beginning of the file
  inout.seekg(0);

  // extract the first line
  inout.getline(p,100);

  // output the first line to stdout
  cout << endl << "Deutch :" << endl;
  cout << p;

  fstream::pos_type pos = inout.tellg();

  // extract the seconf line
  inout.getline(p,100);

  // output the second line to stdout
  cout << endl << "Francais :" << endl;
  cout << p;

  // extract the third line
  inout.getline(p,100);

  // output the third line to stdout
  cout << endl << "English :" << endl;
  cout << p;

  // move the put sequence before
  // the second line
  inout.seekp(pos);

  // replace the second line
  inout << "This is the story of a man" << endl;  

  // replace the third line
  inout << "C'est l'histoire d'un home";

  // seek to the beginning of the file
  inout.seekg(0);

  // output the all content of the
  // fstream object to stdout
  cout << endl << endl << inout.rdbuf();
  cout << endl;

  return 0;
}
