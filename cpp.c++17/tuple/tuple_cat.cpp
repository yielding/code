#include <iostream>
#include <tuple>
#include <string>

using namespace std;

template <typename Tuple, size_t N> 
struct TuplePrinter
{
  static void print(Tuple const& t)
  {
    TuplePrinter<Tuple, N-1>::print(t);
    cout << ", " << get<N-1>(t); 
  }
};

template <typename Tuple>
struct TuplePrinter<Tuple, 1>
{
  static void print(Tuple const& t)
  {
    cout << get<0>(t);
  }
};

template <typename... Args> 
void print(const tuple<Args...>& t)
{
  cout << "(";
  TuplePrinter<decltype(t), sizeof...(Args)>::print(t);
  cout << ")\n";
}

int main(int argc, const char *argv[])
{
  tuple<int, string, float> t1(10, "Test", 3.14);
  int n = 9;
  auto t2 = tuple_cat(t1, make_pair("Foo", "bar"), t1, tie(n));
  n = 10;
  print(t2);
  
  return 0;
}
