#include <thread>
#include <iostream>

using namespace std;

void write_sum(int size, int* data, int& res)
{
  for (size_t i=0; i<size; i++) res += data[i]; 
}

int main(int argc, char const* argv[])
{
  int arr[] = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };
  int res   = 0;
  thread t(write_sum, 10, arr, ref(res));
  t.join();

  cout << res;
  
  return 0;
}
