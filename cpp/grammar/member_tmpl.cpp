#include <iostream>

using namespace std;

template <typename T> 
class X
{
public:
  template <typename U> 
  void search(U const& key);

};

template <typename T> template <typename U> 
void X<T>::search(U const& key)
{
   key.print();
}

struct CatalogKey
{
  void print() const { cout << "catalog key is printed\n"; }
};

int main(int argc, char const* argv[])
{
  X<int> x;
  CatalogKey key;

  x.search(key);

  return 0;
}
