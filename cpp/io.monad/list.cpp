#include <iostream>
#include <vector>
#include <algorithm>
#include <functional>

using namespace std;

template <typename A, typename B>
auto fmap(function<B(A)> f, vector<A> v)
{
  vector<B> w;
  transform(begin(v), end(v), back_inserter(w), f);

  return w;
}

int main(int argc, char* argv[])
{
  vector<int> v{1, 2, 3, 4};

  function<int(int)> f =[](int i) { return i*i; };
  auto w = fmap(f, v);

  return 0;
}