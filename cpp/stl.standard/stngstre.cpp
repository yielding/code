#include<iostream>
#include<sstream>

int main()
{
  using namespace std;

#ifndef _RWSTD_NO_WIDE_CHAR
  // create a bi-directional wstringstream object 
  wstringstream inout;

  // output characters
  inout << L"Das ist die rede von einem man" << endl;
  inout << L"C'est l'histoire d'un home" << endl;
  inout << L"This is the story of a man" << endl;

  wchar_t p[100];

  // extract the first line
  inout.getline(p,100);

  // output the first line to stdout
  wcout << endl << L"Deutch :" << endl;
  wcout << p;

  // extract the seconf line
  inout.getline(p,100);

  // output the second line to stdout
  wcout << endl << L"Francais :" << endl;
  wcout << p;

  // extract the third line
  inout.getline(p,100);

  // output the third line to stdout
  wcout << endl << L"English :" << endl;
  wcout << p;

  // output the all content of the
  //wstringstream object to stdout
  wcout << endl << endl << inout.str();
#else

  // create a bi-directional stringstream object 
  stringstream inout;

  // output characters
  inout << "Das ist die rede von einem man" << endl;
  inout << "C'est l'histoire d'un home" << endl;
  inout << "This is the story of a man" << endl;

  char p[100];

  // extract the first line
  inout.getline(p,100);

  // output the first line to stdout
  cout << endl << "Deutch :" << endl;
  cout << p;

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

  // output the all content of the
  //stringstream object to stdout
  cout << endl << endl << inout.str();
#endif

  return 0;
}

