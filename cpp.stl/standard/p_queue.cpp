#include <queue>
#include <deque>
#include <vector>
#include <string>

#include <iostream>

int main ()
{
  using namespace std;
  //
  // Make a priority queue of int  using a vector container.
  //
  priority_queue<int, vector<int> > pq;
  // 
  // Push a couple of values.
  //
  pq.push(1);
  pq.push(2);
  //
  // Pop a couple of values and examine the ends.
  //
  cout << pq.top() << endl;
  pq.pop();
  cout << pq.top() << endl;
  pq.pop();
  //
  // Make a priority queue of strings.
  //
  priority_queue< string,deque<string> > pqs;
  //
  // Push on a few strings then pop them back off.
  //
  int i;
  for (i = 0; i < 10; i++)
  {
    pqs.push(string(i+1,'a'));
    cout << pqs.top() << endl;
  }
  for (i = 0; i < 10; i++)
  {
    cout << pqs.top() << endl;
    pqs.pop();
  }
  //
  // Make a priority queue of strings using greater.
  //
  priority_queue<string, deque<string>, greater<string> > pgqs;
  //
  // Push on a few strings then pop them back off.
  //
  for (i = 0; i < 10; i++)
  {
    pgqs.push(string(i+1,'a'));
    cout << pgqs.top() << endl;
  }
  for (i = 0; i < 10; i++)
  {
    cout << pgqs.top() << endl;
    pgqs.pop();
  }

  return 0;
}
