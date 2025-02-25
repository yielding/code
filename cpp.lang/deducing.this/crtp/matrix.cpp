#include <print>

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
    println("Matrix.exec(): default impl");
  }
};

class SymmetricMatrix: public Matrix
{
public:
  void exec() const
  {
    println("SymmetricMatrix.exec()");
  }
};

class AntiSymmetricMatrix: public Matrix
{
public:
  void exec() const
  {
    println("AntiSymmetricMatrix.exec()");
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
    print("DifferentStrange.calc(): ");
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

  println(); println();

  sum2(a);
  sum2(b);
  sum2(c);
  sum2(d);

  println(); println();

  a.calc();
  b.calc();
  c.calc();
  d.calc();

  return 0;
}
