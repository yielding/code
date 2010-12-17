#include <iostream>
#include <sstream>
#include <iomanip>

int main ( )
{
  using namespace std;

  float f = 3.14159123;
  int   i = 22;
  char* s = "Why, one can hear the grass growing!";

  // create a read/write stringbuf object on tiny char
  // and attach it to an istringstream object
  istringstream in(ios_base::in | ios_base::out);

  // tie the ostream object to the istringstream object
  ostream out(in.rdbuf());

  out << "test beginning !" << endl;

  // output i in hexadecimal
  out << hex << i <<endl; 

  // set the field width to 10
  // set the padding character to '@'
  // and output i in octal
  out << setw(10) << oct << setfill('@') << i << endl;

  // set the precision to 2 digits after the separator
  // output f
  out << setprecision(3) << f << endl;

  // output the 17 first characters of s
  out.write(s,17);

  // output a newline character
  out.put('\n');

  // output s 
  out << s << endl;   

  // output the all buffer to standard output
  cout << in.rdbuf();     

  return 0;
}
