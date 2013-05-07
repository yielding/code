#include <iostream>
#include <vector>
#include <map>
#include <string>

using namespace std;

struct Foo 
{
  // list-initialization of a non-static member
  vector<int> mem = { 1, 2, 3 }; 
  vector<int> mem2;

  // list-initialization of a member in constructor
  Foo() : mem2{ -1, -2, -3 } 
  {
  } 
};

pair<string, string> f(pair<string, string> p)
{
  return { p.second, p.first }; // list-initialization in return statement
}

int main()
{
  int n0 {};                        // value-initialization (to zero)
  int n1 {1};                       // direct-list-initialization
  string s1 { 'a', 'b', 'c', 'd'};  // initializer-list constructor call
  string s2 { s1, 2, 2 };           // regular constructor call
  string s3 { 0x61, 'a'};           // initializer-list ctor is preferred to (int, char)

  int n2 = { 1 };                   // copy-list-initialization
  double d = double{1.2};           // list-initialization of a temporary, then copy-init

  map<int, string> m = {            // nested list-initialization
    { 1, "a" },
    { 2, {'a', 'b', 'c'} },
    { 3, s1 }
  };

  cout << f({ "hello", "world" }).first // list-initialization in function call
       << '\n';

  const int (&ar)[2] = { 1, 2 }; // binds a lvalue reference to a temporary array
  int&& r1 = {1};                // binds a rvalue reference to a temporary int
  //  int& r2 = {2}; // error: cannot bind rvalue to a non-const lvalue ref

  //  int bad{1.0}; // error: narrowing conversion
  unsigned char uc1{ 10 }; // okay
  //  unsigned char uc2{-1}; // error: narrowing conversion

  Foo f;

  cout << n0 << ' ' << n1 << ' ' << n2 << '\n'
       << s1 << ' ' << s2 << ' ' << s3 << '\n';

  for (auto p: m)
      cout << p.first << ' ' << p.second << '\n';

  for (auto n: f.mem)
      cout << n << ' ';

  for (auto n: f.mem2)
      cout << n << ' ';
}
