#include <cppcoro/generator.hpp>
#include <cppcoro/fmap.hpp>
#include <iostream>

using namespace cppcoro;
using namespace std;

// 1부터 무한히 자연수 생성
auto naturals() -> generator<int> 
{
  for (int i = 1;; ++i)
    co_yield i;
}

int main() 
{
  // 파이프 조합: naturals() → 제곱으로 변환 → 짝수만 선택
  auto squares = naturals()
    | fmap([](int x) { return x * x; })
    | fmap([](int x) -> optional<int> {
        if (x % 2 == 0) return x;  // 짝수만 남김
        return nullopt;            // 나머지는 건너뜀
      });

  int count = 0;
  for (auto v : squares) 
  {
    if (v.has_value()) 
    {
      cout << v.value() << " ";
      if (++count >= 10) break; // 앞 10개만 출력
    }
  }

  cout << "\n";
}
