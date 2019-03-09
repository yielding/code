#include <string>
#include <iostream>
#include <utility>
#include <iomanip>

using namespace std;

struct A
{
  string s;
  int k;

  A(): s("test"), k(-1) { }
  A(const A& o): s(o.s), k(o.k) { cout << "move failed!\n"; }

  A(A && o) noexcept :
    s(move(o.s)),
    k(exchange(o.k, 0))
    {}
};

A f(A a)
{
  return a;
}

struct B : A
{
  string s2;
  int n;
  // implicit move constructur B::(B&&)
  // calls A's move ctor
  // calls s2's move constrctor
  // and make a bitwise copy of n
};

struct C : B
{
  ~C() { } // destructor prevents implicit move ctor C::(C&&)
};

struct D : B
{
  D() {}   
 ~D() {}            // destructor would prevent implicit move ctor D::(D&&)
  D(D&&) = default; // forces a move ctor anyway
};

int main(int argc, char *argv[])
{
  cout << "Trying to move A\n";
  A a1 = f(A()); // return by value move-constructs the target from the function parameter
  cout << "Befoer move, a1.s = " << quoted(a1.s) << " a1.k = " << a1.k << endl;
  A a2 = move(a1); // move-constructs from xvalue
  cout << " After move, a1.s = " << quoted(a1.s) << " a1.k = " << a1.k << endl;
  cout << endl;

  cout << "Trying to move B\n";
  B b1;
  cout << "Before move b1.s = " << quoted(b1.s) << endl;
  B b2 = move(b1); // calls implicit move-ctor
  cout << " After move, b1.s = " << quoted(b1.s) <<  endl;
  cout << endl;

  cout << "Trying to move C\n";
  C c1;
  C c2 = move(c1); // calls copy-ctor
  cout << endl;

  cout << "Trying to move D\n";
  D d1;
  cout << "Before move d1.s = " << quoted(d1.s) << endl;
  D d2 = move(d1); // calls copy-ctor
  cout << " After move, d1.s = " << quoted(d1.s) <<  endl;
  cout << endl;

  return 0;
}
