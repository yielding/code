#include <stack>
#include <vector>
#include <iostream>

using namespace std;

int main()
{
    vector<int> input { 5, 4, 3, 2, 1};
    stack <int> numbers;

    for (auto no: input)
        numbers.push(no);

    while (!numbers.empty())
    {
        cout << numbers.top() << endl;
        numbers.pop();
    }

    return 0;
}
