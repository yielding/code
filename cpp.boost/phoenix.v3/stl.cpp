#include <iostream>
#include <vector>
#include <algorithm>
#include <boost/phoenix.hpp>

using namespace std;
      namespace phoenix = boost::phoenix;
      using     phoenix::arg_names::arg1;

int main(int argc, char const* argv[])
{
    int arr[] = { 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 };
    vector<int> v (arr, arr+10);

    cout << phoenix::size(arg1)(v) << endl;
    cout << phoenix::at(arg1, 5)(v);

    return 0;
}
