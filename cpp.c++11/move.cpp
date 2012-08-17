#include <iostream>
#include <utility>
#include <vector>
#include <string>

using namespace std;

int main()
{
  string str = "Hello";
  vector<string> v;

  // uses the push_back(const T&) overload, which means 
  // we'll incur the cost of copying str
  v.push_back(str);
  cout << "After copy, str is \"" << str << "\"\n";

  // uses the rvalue reference push_back(T&&) overload, 
  // which means no strings will copied; instead, the contents
  // of str will be moved into the vector.  This is less
  // expensive, but also means str might now be empty.
  v.push_back(move(str));
  cout << "After move, str is \"" << str << "\"\n";

  cout << "The contents of the vector are \"" << v[0]
       << "\", \"" << v[1] << "\"\n";
}
