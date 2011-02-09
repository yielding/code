#include <iostream>

using namespace std;

template <typename T> 
class object_counter
{
public:
  static size_t live()          { return m_count; }

protected:
  object_counter()              { ++m_count; }
  object_counter(object_counter const&) { ++m_count; }
  ~object_counter()             { --m_count; }

private:
   static size_t m_count;
};

template <typename T>
size_t object_counter<T>::m_count = 0;

class String: public object_counter<String>
{};

int main(int argc, char const* argv[])
{
  {
  String s1, s2;

  cout << String::live();
  }

  cout << String::live();
  
  return 0;
}
