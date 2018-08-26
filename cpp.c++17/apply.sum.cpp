#include <iostream>

template <typename ...F> 
void ApplyAll(int n, const F&... f) 
{   
  (f(n), ...); // unary fold (over the comma operator) 
}  

// Compute f(a, b) for each f in the pack and return the sum. 
template <typename ...F> 
int ApplyAndSum(int a, int b, const F&... f) 
{   
  return (f(a, b) + ... + 0); // binary fold 
}

struct add_f
{
  int operator() (int a, int b) const
  {
    return a + b;
  }
};

int main(int argc, char *argv[])
{
  auto sub_f  = [](int a, int b) { return a - b; };

  auto result = ApplyAndSum(1, 2, add_f(), sub_f);

  std::cout << result << std::endl;

  return 0;
}
