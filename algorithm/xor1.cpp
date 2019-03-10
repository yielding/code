#include <iostream>

using namespace std;

int main(int argc, char *argv[])
{
  int value = 12345;
  int key   = 0b11001100;

  int cipher_text = value       xor key;
  int plain_text  = cipher_text xor key;

  cout << cipher_text << endl;
  cout << plain_text  << endl;
  
  return 0;
}
