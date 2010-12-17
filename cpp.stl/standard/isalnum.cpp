
#include <iostream>

int main ()
{
  using namespace std;

  locale loc;

  cout << isalnum('a',loc) << endl;
  cout << isalnum(',',loc) << endl;

  return 0;
}
