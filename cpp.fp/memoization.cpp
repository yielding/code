#include <iostream>
#include <map>
#include <tuple>

using namespace std;

template <typename Result, typename... Args> 
auto make_memoized(Result (*f)(Args...))
{ 
  map<tuple<Args...>, Result> cache;

  return [f, cache](Args... args) mutable -> Result {
    const auto args_tuple = make_tuple(args...); 
    const auto cached = cache.find(args_tuple);

    if (cached == cache.end()) 
    {
      auto result = f(args...); cache[args_tuple] = result;
      return result;
    } 
    else 
    { 
      return cached->second; 
    }
  };
}

unsigned int fib(unsigned int n) 
{ 
  return n == 0 ? 0 :
         n == 1 ? 1 :
                  fib(n - 1) + fib(n - 2); 
}

template <typename F> 
unsigned int fib2(F&& fibmemo, unsigned int n) 
{ 
  return n == 0 ? 0 : 
         n == 1 ? 1 : 
                  fibmemo(n - 1) + fibmemo(n - 2); 
}

int main(int argc, char* argv[])
{
  auto fibo = make_memoized(fib2);
  cout << fibo(40);

  return 0;
}