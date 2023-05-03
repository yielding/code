#include "library.h"

MyStack::MyStack()
{}

int MyStack::top()
{
  static int a = 1;
  return a++;
}

int libFunc()
{
  MyStack s;
  return s.top();
}
