#include <algorithm>
#include <chrono>
#include <cstdlib>
#include <functional>
#include <iostream> 
#include <vector>

using namespace std;

template<typename It, class Compare>
void insertion_sort(It beg, It end, Compare comp)
{
  auto length = distance(beg, end);

  auto arr = beg; 
  for (int i=1; i<length; i++)
    for (int j=i; j>0 && comp(arr[j], arr[j-1]); j--)
      swap(arr[j], arr[j-1]);
}

void p(const vector<int>& arr)
{
  for (int i=0; i<10; i++) cout << arr[i] << " ";
  cout << endl;
}

template<class Compare>
void test_insertion(vector<int> arr, Compare comp)
{
  auto t0 = chrono::high_resolution_clock::now();
  insertion_sort(arr.begin(), arr.end(), comp);
  auto t1 = chrono::high_resolution_clock::now();
  
  auto d = chrono::duration<double, milli>(t1 - t0).count();
  cout << d << endl;

  p(arr);
}

template<class Compare>
void test_stdsort(vector<int> arr, Compare comp)
{
  auto t0 = chrono::high_resolution_clock::now();
  sort(arr.begin(), arr.end(), comp);
  auto t1 = chrono::high_resolution_clock::now();
  
  auto d = chrono::duration<double, milli>(t1 - t0).count();
  cout << d << endl;

  p(arr);
}

int main(int argc, char *argv[])
{
  const int SZ = 30;
  srand(time(nullptr));
  vector<int> arr = { 30, 28, 39, 27, 26, 25, 24, 23, 22, 21, 
                      20, 18, 19, 17, 16, 15, 14, 13, 12, 11, 
                       8,  9,  7,  6,  5,  4,  3,  2,  1,  0 };


  //for (auto i=0; i<SZ; i++) arr.push_back(rand() % SZ);
  // for (auto i=0; i<SZ; i++) arr.push_back(i);
  for (auto i: arr) cout << i << " ";
  cout << endl;

  //for (auto i=0; i<SZ; i++) arr.push_back(SZ-i);

  test_insertion(arr, greater<int>());
  test_stdsort  (arr, greater<int>());

  return 0;
}
