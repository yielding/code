#include <iostream>
#include <map>
#include <tuple>

using namespace std;

// Y combinator for self-referencing functions
template <typename F>
auto make_recursive_memoized(F&& f) 
{
  map<uint64_t, uint64_t> cache;
  
  return [f, cache](auto&& self, uint64_t n) mutable -> uint64_t {
    cout << "fibo(" << n << ") 호출\n";
    
    auto cached = cache.find(n);
    if (cached != cache.end()) 
    {
      cout << "  → 캐시 hit: " << cached->second << "\n";
      return cached->second;
    }
    
    cout << "  → 계산 중...\n";
    auto result = f(self, n);
    cache[n] = result;
    cout << "  → 결과: " << result << " (캐시에 저장)\n";
    return result;
  };
}

int main(int argc, char* argv[])
{
  cout << "=== 개선된 make_recursive_memoized ===\n";
  auto fibo_recursive = make_recursive_memoized([](auto&& self, uint64_t n) {
    return n == 0 ? 0 : 
           n == 1 ? 1 : 
           self(self, n - 1) + self(self, n - 2);
  });
  
  cout << "최종 결과: " << fibo_recursive(fibo_recursive, 4) << endl;

  return 0;
}