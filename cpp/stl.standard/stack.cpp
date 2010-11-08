#include <stack>
#include <vector>
#include <deque>
#include <string>

#include <iostream>

int main ()
{
  using namespace std;

  //
  // Make a stack using a vector container.
  //
  stack< int, vector<int> > s;
  //
  // Push a couple of values on the stack.
  //
  s.push(1);
  s.push(2);
  cout << s.top() << endl;
  //
  // Now pop them off.
  //
  s.pop();
  cout << s.top() << endl;
  s.pop();
  //
  // Make a stack of strings using a deque.
  //
  stack< string, deque<string> > ss;
  //
  // Push a bunch of strings on then pop them off.
  //
  for (int i = 0; i < 10; i++)
  {
    ss.push(string(i+1,'a'));
    cout << ss.top() << endl;
  }
  for (int i = 0; i < 10; i++)
  {
    cout << ss.top() << endl;
    ss.pop();
  }

  return 0;
}
