#include <iostream>
#include <expected> 

using namespace std;

enum class error 
{
  compile_time_error,
  runtime_error
};

auto unexpected_runtime_error() -> expected<int, error>
{
  return unexpected(error::runtime_error);
}

int main(int argc, char* argv[])
{
  expected<double, int> ex = unexpected(3);

  if (!ex) 
    cout << "ex contains an error\n";

  if (ex == unexpected(3))
    cout << "The error value is equal to 3\n";

  const auto e = unexpected_runtime_error();
  e.and_then([](const auto& e) -> expected<int, error>
  {
    cout << "and_then" << int(e);
    return {};
  })
  .or_else([](const auto& e) -> expected<int, error>
  {
    cout << "or_else: " << int(e);
    return {};
  });

  return 0;
}
