#include <iostream>
#include <string>
#include <tuple>

using namespace std;

// NOTE
// 이 Writere Moand 예제는 kleisli compostion
// (a -> M a) -> (b -> M b) -> (c -> M c)
//
template<typename A>
using Writer = pair<A, string>;

template<typename A>
auto identity(A x) -> Writer<A> 
{
  return make_pair(x, "");
}

auto const compose = [](auto m1, auto m2) 
{
  return [m1, m2](auto x) {
    auto p1 = m1(x);
    auto p2 = m2(p1.first);
    return make_pair(p2.first, p1.second + p2.second);
  };
};

auto words(string s) -> vector<string> 
{
  vector<string> result{""};

  for (auto e : s)
  {
    if (isspace(e))
      result.push_back("");
    else
      result.back() += e;
  }

  return result;
}

auto toUpper(string s) -> Writer<string> 
{
  int (*toupperp)(int) = &toupper;

  string result;
  transform(begin(s), end(s), back_inserter(result), toupperp);

  return make_pair(result, "toUpper ");
}

auto toWords(string s) -> Writer<vector<string>> 
{
  return make_pair(words(s), "toWords ");
}

auto process(string s) -> Writer<vector<string>> 
{
   return compose(toUpper, toWords)(s);
}

int main(int argc, char* argv[])
{
  auto res = process("I want to know what love is");
  cout << res.second << endl;

  for (auto& e: res.first)
    cout << e << endl;

  return 0;
}
