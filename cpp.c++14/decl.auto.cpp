#include <vector>
#include <iostream>

using namespace std;

void authenticateUser()
{
}

// NOTE
template <typename Container, typename Index>
decltype(auto)
authAndAccess(Container&& c, Index i)
{
  authenticateUser();
  return forward<Container>(c)[i];
}

int main(int argc, char *argv[])
{
  vector<int> v{1, 2, 3};

  authAndAccess(v, 0) = 2;

  cout << v[0];
  
  return 0;
}
