#include <torch/torch.h>
#include <iostream>

using namespace std;

int main(int argc, char *argv[])
{
  auto tensor = torch::eye(3);

  cout << tensor << endl;
  
  return 0;
}
