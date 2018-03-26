#include <iostream>

using namespace std;

template <typename T> 
class Stack
{
public:
  Stack() {}

  void push() {
  }

  T top()
  {
    return T();
  }

private:
  int m_top = 0;
};

int main(int argc, const char *argv[])
{
  return 0;
}
