#include <iostream>
#include <string>
#include <functional>
#include <unordered_map>
#include <map>
#include <memory>

using namespace std;

template <typename ReturnType, typename... Args>
std::function<ReturnType (Args...)>
memoize(ReturnType (*func) (Args...))
{
    auto cache = make_shared<map<tuple<Args...>, ReturnType>>();
    return (
        [=](Args... args) mutable {
            tuple<Args...> t(args...);

            if (cache->find(t) == cache->end()) (*cache)[t] = func(args...);

            return (*cache)[t];
        }
    );
}

template <typename F_ret, typename...  F_args>
function<F_ret (F_args...)>
memoized_recursion(F_ret (*func)(F_args...))
{
    typedef function<F_ret (F_args...)> FunctionType;
    static unordered_map<decltype(func), FunctionType> functor_map;

    if (functor_map.find(func) == functor_map.end())
        functor_map[func] = memoize(func);

    return functor_map[func];
}

unsigned long fibonacci(unsigned n)
{
    return (n < 2) ? n :
        memoized_recursion(fibonacci)(n - 1) +
        memoized_recursion(fibonacci)(n - 2);
}

unsigned long fibonacci_slow(unsigned n)
{
    return (n < 2) ? n :  fibonacci_slow(n - 1) + fibonacci_slow(n - 2);
}

int main(int argc, char *argv[])
{
    unsigned count = 3000;
    cout << memoized_recursion(fibonacci)(count) << endl;
    cout << fibonacci(count) << endl;

    return 0;
}
