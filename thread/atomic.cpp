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
  for (int i = 1; i <= 20; i++)
    ths.push_back(thread(&square, i, ref(accum)));

  for (auto& th : ths)
    th.join();

  cout << "accum = " << accum << endl;
  return 0;
}
