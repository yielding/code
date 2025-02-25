#include <print>

using namespace std;

int main(int argc, char* argv[])
{
  auto fact = [](this auto&& self, int n) -> int {
    return (n == 0) ? 1 : n * self(n - 1);
  };

  println("Factorial of 5: {}", fact(5));

  return 0;
}
