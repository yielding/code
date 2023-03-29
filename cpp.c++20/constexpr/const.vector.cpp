#include <iostream>

using namespace std;

class Vector
{
public:
  constexpr Vector(int x, int y): x_{x}, y_{y} {}

  constexpr int x() const { return x_; }
  constexpr int y() const { return y_; }

private:
  int x_;
  int y_;
};

constexpr 
auto add_vector(const Vector& v1, const Vector& v2) -> Vector
{
  return {v1.x() + v2.x(), v1.y() + v2.y()};
}

template <int N>
struct A
{
  int operator()() { return N; }
};

int main(int argc, char *argv[])
{
  constexpr Vector v1{1, 2};
  constexpr Vector v2{2, 3};

  A<v1.x()> a;

  cout << a() << endl;

  A<add_vector(v1, v2).x()> b;

  cout << b() << endl;
  
  return 0;
}