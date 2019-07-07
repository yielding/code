#include <iostream>
#include <vector>
#include <string>

using namespace std;

enum class Color { Red, Green, Blue };
enum class Size  { Small, Medium, Large };

struct Product
{
  string name;
  Color  color;
  Size   size;
};

template <typename T> class AndSpec;
template <typename T> class OrSpec;

template <typename T>
struct Spec
{
  virtual auto is_satisfied(T* item) -> bool = 0;
  
  auto operator && (Spec&& other) -> AndSpec<T>
  {
    return AndSpec<T>(*this, other);
  }
  
  auto operator || (Spec&& other) -> OrSpec<T>
  {
    return OrSpec<T>(*this, other);
  }
};

template <typename T>
struct AndSpec : Spec<T>
{
  Spec<T>& first;
  Spec<T>& second;

  AndSpec(Spec<T>& first, Spec<T>& second): first{first}, second{second} {}
  
  auto is_satisfied(T* item) -> bool override
  {
    return first.is_satisfied(item) && second.is_satisfied(item);
  }
};

template <typename T>
struct OrSpec : Spec<T>
{
  Spec<T>& first;
  Spec<T>& second;
  
  OrSpec(Spec<T>& first, Spec<T>& second): first{first}, second{second} {}
  
  auto is_satisfied(T* item) -> bool override
  {
    return first.is_satisfied(item) || second.is_satisfied(item);
  }
};

template <typename T>
struct Filter
{
  virtual auto filter(vector<T*> items, Spec<T>& spec) -> vector<T*> = 0;
};

struct BetterFilter : Filter<Product>
{
  auto filter(vector<Product*> items, Spec<Product>& spec) -> vector<Product*> override
  {
    vector<Product*> result;
    
    for (auto& p: items)
    {
      if (spec.is_satisfied(p)) result.push_back(p);
    }

    return result;
  }
};

struct ColorSpec: Spec<Product>
{
  Color color;
  
  explicit ColorSpec(const Color color) : color{color}
  {}
  
  auto is_satisfied(Product* item) -> bool override
  {
    return item->color == color;
  }
};

struct SizeSpec: Spec<Product>
{
  Size size;
  
  explicit SizeSpec(const Size size) : size{size}
  {}
  
  auto is_satisfied(Product* item) -> bool override
  {
    return item->size == size;
  }
};

//------------------------------------------------------------------------------
//
//
//
//------------------------------------------------------------------------------
int main(int argc, const char * argv[])
{
  Product apple {"Apple", Color::Green, Size::Small };
  Product tree  {"Tree",  Color::Green, Size::Large };
  Product house {"House", Color::Blue,  Size::Large };
  
  vector<Product*> all { &apple, &tree, &house };
  
  BetterFilter bf;
  auto green_and_large = ColorSpec(Color::Green) && SizeSpec(Size::Large);
  auto green_or_large  = ColorSpec(Color::Green) || SizeSpec(Size::Large);
  
  auto green_things = bf.filter(all, green_and_large);
  for (auto& x: green_things)
    cout << x->name << " " << "is green" << endl;
  
  auto green_or_large_things = bf.filter(all, green_or_large);
  for (auto& x: green_or_large_things)
    cout << x->name << " " << "is green or large" << endl;

  return 0;
}

//------------------------------------------------------------------------------
//
//
//
//------------------------------------------------------------------------------
