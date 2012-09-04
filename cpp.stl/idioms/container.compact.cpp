#include <vector>
#include <iostream>

using namespace std;

int main()
{
    vector<int> v = { 1, 2, 3, 4, 5 };

    v.erase(v.begin());
    vector<int> v1(v);

    cout << "org. v capacity: " 
         << v.capacity() << endl;

    cout << "copy v capacity: " 
         << v1.capacity() << endl;

    vector<int>(v).swap(v);

    cout << "shirinked v. capacity: " 
         << v.capacity()  << endl;

    for (auto i: v) cout << i << " ";

    cout << endl;

    return 0;
}
