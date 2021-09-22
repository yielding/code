#include <algorithm>
#include <iostream>
#include <ranges>
#include <string_view>

#define lazy_split_view split_view
#define lazy_split split

using namespace std;

auto print = [](auto const& view)
{
  // `view` is a views::lazy_split_view::/*outer-iterator*/::value_type

  for (cout << "{ "; const auto element : view)
    cout << element << ' ';

  cout << "} ";
};

int main()
{
  constexpr static auto source = { 0, 1,0, 2,3,0, 4,5,6,0, 7,8,9 };
  constexpr int delimiter {0};
  constexpr ranges::lazy_split_view lazy_split_view {source, delimiter};
  cout << "splits[" 
       << ranges::distance(lazy_split_view) 
       << "]:  ";

  for (auto const& lazy_split: lazy_split_view)
    print(lazy_split);

  constexpr string_view hello { "Hello C++ 20 !" };
  cout << "\n" "substrings: ";
  ranges::for_each(hello | views::lazy_split(' '), print);

  constexpr string_view text { "Hello-+-C++-+-20-+-!" };
  constexpr string_view delim { "-+-" };
  cout << "\n" "substrings: ";

  ranges::for_each(text | views::lazy_split(delim), print);

  return 0;
}
