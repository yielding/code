#include <iostream>
#include <vector>
#include <algorithm>
#include <span>

using namespace std;

auto print(span<int> container) -> void
{
  cout << "c.size(): " << container.size() << endl;

  for (auto e: container) cout << e << " ";
  cout << "\n\n";
}


int main(int argc, char *argv[])
{
  vector v{1, 2, 3, 4, 5, 6};
  print(v);

  span s1(v);
  print(s1);

  span s2{s1.subspan(1, s1.size()-2)};
  print(s2);

  transform(s2.begin(), s2.end(),
    s2.begin(),
    [](int i) { return i * i; }); 

  print(s2);
  print(v);


  return 0;
}
