#ifndef STACK_HPP
#define STACK_HPP

#include <vector>

template <typename T>
class Stack {
public:
  Stack();
  void push(T& elem);
  T pop();
private:
  std::vector<T> elems;
};

#endif
