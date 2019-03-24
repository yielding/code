#include <iostream>
#include <vector>
using namespace std; 
struct foo
{
  int m_value;

  explicit foo(int val) : m_value(val) 
  {
    cout << "foo(" << m_value << ")\n";
  }

  foo(foo const& rhs)
  {
    if (this != &rhs)
    {
      m_value = rhs.m_value;
      cout << "foo(foo(" << m_value << "))\n";
    }
  }

  foo(foo&& rhs) noexcept 
  {
    m_value = move(rhs.m_value);
    rhs.m_value = -1;
    cout << "foo(move(foo(" << m_value << "))\n";
  }

  ~foo()
  {
    if (m_value != -1)
      cout << "~foo(" << m_value << ")\n";
  }
};

int main(int argc, char *argv[])
{
  {
  vector<foo> foos;
  for (int i=0; i<10; i++) foos.emplace_back(i);
  }

  {
  vector<foo> foos;
  foos.reserve(10);
  for (int i=0; i<10; i++) foos.emplace_back(i);
  }

  return 0;
}
