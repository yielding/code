#include <iostream>

class Base 
{
public:
  void interface(this auto&& self) 
  {
    self.implementation();
  }

  void implementation() 
  {
    std::cout << "Called from Base\n";
  }
};

class Derived : public Base 
{
public:
  void implementation() 
  {
    std::cout << "Called from Derived (with deducing this)\n";
  }
};

int main() 
{
  Base b; b.interface();
  Derived d; d.interface();
}
