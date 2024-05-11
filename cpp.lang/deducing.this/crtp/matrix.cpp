#include <iostream>

using namespace std;

class Matrix 
{
public:
  template<typename Self>
  void calc(this Self&& self)
  {
    self.exec();
  }

  void exec() const 
  {
    cout << "Matric.exec(): default impl" << endl;
  }
};

class SymmetricMatrix: public Matrix
{
public:
  void exec() const
  {
    cout <<"SymmetricMatrix::exec" << endl;
  }
};

class AntiSymmetricMatrix: public Matrix
{
public:
  void exec() const
  {
    cout <<"Anti SymmetricMatrix::exec" << endl;
  }
};

class SingularMatrix: public Matrix
{
public:
};

class DifferentStrange
{
public:
  void calc()
  {
    cout <<"DifferentStrange: " << endl;
  }
};

void sum(Matrix& a)
{
  a.calc();
}

template <class Leaf>
void sum2(Leaf& a)
{
  a.calc();
}

int main()
{
  SymmetricMatrix a;
  AntiSymmetricMatrix b;
  SingularMatrix c;
  DifferentStrange d;

  sum(a);
  sum(b);
  sum(c);
  // sum(d); // compile error

  cout << "\n";

  sum2(a);
  sum2(b);
  sum2(c);
  sum2(d);

  return 0;
}
