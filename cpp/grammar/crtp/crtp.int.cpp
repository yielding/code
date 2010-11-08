#include <cassert>

template<class T>
class ordered 
{
public:
  bool operator > (T const& rhs) const 
  {
    T const& self = static_cast<T const&>(*this);
    return rhs < self;
  }
};

class Int: public ordered<Int> 
{
public:
  explicit Int(int x): value(x) {}
  bool operator < (Int const& rhs) const 
  {
    return this->value < rhs.value;
  }
  int value;
};

int main()
{
  assert(Int(4) < Int(6));
  assert(Int(9) > Int(4));

  return 0;
}
