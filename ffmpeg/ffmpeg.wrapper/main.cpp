#include <print>
#include <cstdlib>

using namespace std;

int main(int argc, char* argv[])
{
  auto home = getenv("HOME");
  println("Hello, my home is {}", home);

  return 0;
}

