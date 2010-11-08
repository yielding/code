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
    std::cout << "Matric.exec() " << std::endl;
  }
};

class SymmetricMatrix: public Matrix<SymmetricMatrix> 
{
public:
  void exec() const
  {
    std::cout <<"SymmetricMatrix::do" << std::endl;
  }
};

class AntiSymmetricMatrix: public Matrix<AntiSymmetricMatrix> 
{
public:
  void exec() const
  {
    std::cout <<"Anti SymmetricMatrix::do" << std::endl;
  }
};

class SingularMatrix: public Matrix<SingularMatrix> 
{
public:
};

template <class Leaf>
void sum(Matrix<Leaf>& a)
{
  a.calc();
}

int main()
{
  SymmetricMatrix a;
  AntiSymmetricMatrix b;
  SingularMatrix c;

  sum(a);
  sum(b);
  sum(c);
}
