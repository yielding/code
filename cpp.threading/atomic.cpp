#include <iostream>
#include <vector>
#include <thread>
#include <atomic>

using namespace std;

//int accum = 0;

void square(int x, atomic<int>& accum) 
{
  accum += x * x;
}

int main() 
{
  vector<thread> ths;
  atomic<int> accum = 0;
  // TODO memorize
  // regardless of ref or not, we have to protect shared variable 'accum'
  // below is just using atomic
  for (int i = 1; i <= 20; i++)
    ths.push_back(thread(&square, i, ref(accum)));

  for (auto& th : ths)
    th.join();

  cout << "accum = " << accum << endl;
  return 0;
}
