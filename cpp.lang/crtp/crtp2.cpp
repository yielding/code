#include <iostream>
#include <cstring>

using namespace std;

template <typename OnType>
class streamer
{
public:
  int byte_count()
  {
    OnType const &s = self();
    return strlen(s.m_name) + strlen(s.m_value) + 2;
  }

private:
  OnType const& self()
  {
    OnType const& self = static_cast<OnType const&>(*this);
    return self;
  } 
};

class char_field: public streamer<char_field>
{
public:
  char_field(char const* name, char const* value)
  {
    m_name  = strdup(name);
    m_value = strdup(value); 
  }

  ~char_field()
  {
    delete m_name;
    delete m_value;
  }

  char* m_name;
  char* m_value;
};

int main()
{
  char_field n("12345", "12345");

  cout << n.byte_count();
}
