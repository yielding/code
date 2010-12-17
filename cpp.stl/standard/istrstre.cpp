#include<iostream>
#include<strstream>

int main ( )
{
  using namespace std;

  const char* p="C'est pas l'homme qui prend la mer, ";
  char* s="c'est la mer qui prend l'homme";

  // create an istrstream object and initialize
  // the underlying strstreambuf with p
  istrstream in_first(p);

  // create an istrstream object and initialize
  // the underlying strstreambuf with s
  istrstream in_next(s);

  // create an strstream object
  strstream out;

  // output the content of in_first and
  // in_next to out
  out << in_first.rdbuf() << in_next.str();

  // output the content of out to stdout
  cout << endl << out.rdbuf() << endl;


  // output the content of in_first to stdout
  cout << endl << in_first.str();

  // output the content of in_next to stdout
  cout << endl << in_next.rdbuf() << endl;

  return 0;
}
