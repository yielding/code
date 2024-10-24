#include <string>
#include <utility>
#include <iostream>
#include <functional>
#include <format>

using namespace std;

template <typename T>
class Writer 
{
public:
  Writer(T value, string log) 
    : value_(value), log_(move(log)) 
  {}

  T value() const { return value_; }
  string log() const { return log_; }

  template <typename Func>
  auto flatMap(Func f) const 
  {
    using ResultWriter = decltype(f(value_));

    auto next = f(value_);
    return ResultWriter(next.value(), log_ + next.log());
  }

private:
  T value_;
  string log_;
};

// 샘플 함수들
auto add(int x, int y) -> Writer<int> 
{
  int result = x + y;

  return Writer<int>(result, format("Added {} and {}\n", x, y));
}

auto multiply(int x, int y) -> Writer<int> 
{
  int result = x * y;

  return Writer<int>(result, format("Multiplied {} and {}\n", x, y));
}

int main() 
{
  auto result = add(3, 4)
    .flatMap([](int sum) { return multiply(sum, 2); })
    .flatMap([](int sum) { return add(sum, 6); })
    ;

  cout << "Result: " << result.value() << "\n";
  cout << "Log:\n" << result.log();

  return 0;
}
