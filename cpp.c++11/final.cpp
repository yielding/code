#include <iostream>

class A final {

};

class B: public A {

};

int main(int argc, char *argv[])
{
  A a;
  
  std::cout << "hello";
  return 0;
}
