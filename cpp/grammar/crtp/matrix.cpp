#include <iostream>

template <class Leaf>
class Matrix 
{
public:
  void calc() 
  {
    Leaf const& self = static_cast<Leaf const&>(*this);
    self.exec();
  }

  void exec() const
  {
    std::cout << "Matric.exec(): default impl" << std::endl;
  }
};

class SymmetricMatrix: public Matrix<SymmetricMatrix> 
{
public:
  void exec() const
  {
    std::cout <<"SymmetricMatrix::exec" << std::endl;
  }
};

class AntiSymmetricMatrix: public Matrix<AntiSymmetricMatrix> 
{
public:
  void exec() const
  {
    std::cout <<"Anti SymmetricMatrix::exec" << std::endl;
  }
};

class SingularMatrix: public Matrix<SingularMatrix> 
{
public:
};

class DifferentStrange
{
public:
  void calc()
  {
    std::cout <<"DifferentStrange: " << std::endl;
  }
};

template <class Leaf>
void sum(Matrix<Leaf>& a)
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
//  sum(d); : compile error

  std::cout << "\n";

  sum2(a);
  sum2(b);
  sum2(c);
  sum2(d);
}
