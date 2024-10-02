#include <iostream>
#include <range/v3/all.hpp>

namespace v = ranges::views;
using namespace std;

int main() 
{
    using v::all, v::split_when;

    auto words = vector{"apple"s, "banana"s, "cherry"s, "date"s, "elderberry"s, "fig"s, "grape"s};

    auto split_ranges = words | split_when([](auto& w) { return w.length() > 5; });
    cout << all(split_ranges) << endl; // [[apple],[date],[fig,grape]]

    auto const s0 = string{"123xyz567"};
    cout << s0 << endl;
    auto rng9 = s0 | v::split_when([](auto c) { return !isdigit(c); });
    cout << v::all(rng9) << endl; // [[1,2,3],[5,6,7]]

    return 0;
}
