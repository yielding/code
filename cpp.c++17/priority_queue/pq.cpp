#include <functional>
#include <queue>
#include <vector>
#include <iostream>

using namespace std;

template <typename T>
void print_queue(T& q)
{
  while (!q.empty()) 
  {
    cout << q.top() << " ";
    q.pop();
  }

  cout << endl;
}

int main(int argc, char *argv[])
{
  priority_queue<int> q;

  for (auto n: { 1, 8, 5, 6, 3, 4, 0, 9, 7, 2})
    q.push(n);

  print_queue(q);

  
  return 0;
}
