#include <vector>

#include <iostream>

int main()
{
  using namespace std;

  cout << "Prime Sieve program, a test of vectors" << endl;
  //
  // Create a sieve of bits, initially on.
  //
  const int sievesize = 100;
  vector<int> sieve(sievesize, 1);
  //
  // Now search for 1 bt positions.
  //
  for (int i = 2; i * i < sievesize; i++)
    if (sieve[i])
      for (int j = i + i; j < sievesize; j += i)
        sieve[j] = 0;
  //
  // Now output all the values that are set.
  //
  for (int j = 2; j < sievesize; j++)
    if (sieve[j]) 
      cout << j << " ";
  cout << endl;

  cout << "End of Prime Sieve program" << endl;

  return 0;
}
