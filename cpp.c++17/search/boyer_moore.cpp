#include <iostream>
#include <string>
#include <algorithm>
#include <functional>

using namespace std;

int main()
{
    string in = "Lorem ipsum dolor sit amet, consectetur adipiscing elit,"
                " sed do eiusmod tempor incididunt ut labore et dolore magna aliqua";
    string needle = "pisci";
    auto it = search(in.begin(), in.end(),
                   std::boyer_moore_horspool_searcher(needle.begin(), needle.end()));
    if (it != in.end())
        cout << "The string " << needle << " found at offset "
             << it - in.begin() << '\n';
    else
        cout << "The string " << needle << " not found\n";
}
