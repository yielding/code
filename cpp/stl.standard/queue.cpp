#include <queue>
#include <string>
#include <deque>
#include <list>

#include <iostream>

int main ()
{
  using namespace std;
  //
  // Make a queue using a deque container.
  //
  queue< int, list<int> > q;
  //
  // Push a couple of values on then pop them off.
  //
  q.push(1);
  q.push(2);
  cout << q.front() << endl;
  q.pop();
  cout << q.front() << endl;
  q.pop();
  //
  // Make a queue of strings using a deque container.
  //
  queue< string, deque<string> > qs;
  //
  // Push on a few strings then pop them back off.
  //
  int i;
  for (i = 0; i < 10; i++)
  {
    qs.push(string(i+1,'a'));
    cout << qs.front() << endl;
  }
  for (i = 0; i < 10; i++)
  {
    cout << qs.front() << endl;
    qs.pop();
  }

  return 0;
}
