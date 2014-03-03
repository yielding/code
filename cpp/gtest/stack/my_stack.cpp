#include "my_stack.h"

stack::stack(int size) 
  : m_size(size)
  , m_top(0)
  , m_data(new int[size])
{
}

stack::~stack()
{
  delete [] m_data;
}

int stack::top()
{
  return m_data[m_top-1];
}

void stack::pop()
{
   m_top--;
}

void stack::push(int data)
{
  m_data[m_top] = data;
  m_top++;
}

int stack::size()
{
  return m_top;
}

