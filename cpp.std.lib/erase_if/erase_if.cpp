#include <iostream>
#include <numeric>
#include <string_view>
#include <list>

using namespace std;

void print_container(string_view comment, const list<char>& c)
{
  cout << comment;
  for (auto x : c) cout << x << ' ';
  
  cout << '\n';
}

int main()
{
  list<char> cnt(10);
  iota(cnt.begin(), cnt.end(), '0');
  print_container("Init:\n", cnt);

  erase(cnt, '3');
  print_container("Erase '3':\n", cnt);

  auto erased = erase_if(cnt, [](char x) { return (x - '0') % 2 == 0; });
  print_container("Erase all even numbers:\n", cnt);
  cout << "In all " << erased << " even numbers were erased.\n";
  cout << "In all " << cnt.size() << " are left.\n";

  return 0;
}
