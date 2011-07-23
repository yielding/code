#include <boost/iterator/counting_iterator.hpp>
#include <boost/iterator/indirect_iterator.hpp>
#include <boost/cstdlib.hpp>
#include <algorithm>
#include <iostream>
#include <iterator>
#include <vector>

using namespace std;
using namespace boost;

int main(int, char*[])
{
    // Example of using counting_iterator
    cout << "counting from 0 to 4:" << endl;
    counting_iterator<int> first(0), last(4);
    copy(first, last, ostream_iterator<int>(cout, " "));
    cout << endl;

    // Example of using make_counting_iterator()
    cout << "counting from -5 to 4:" << endl;
    copy(make_counting_iterator(-5), make_counting_iterator(5),
         ostream_iterator<int>(cout, " "));
    cout << endl;

    // Example of using counting iterator to create an array of pointers.
    int N = 7;
    vector<int> numbers;
    // Fill "numbers" array with [0,N)
    copy(counting_iterator<int>(0), counting_iterator<int>(N), 
         back_inserter(numbers));

    vector<vector<int>::iterator> pointers;

    // Use counting iterator to fill in the array of pointers.
    // causes an ICE with MSVC6
    copy(make_counting_iterator(numbers.begin()),
         make_counting_iterator(numbers.end()),
         back_inserter(pointers));

    // Use indirect iterator to print out numbers by accessing
    // them through the array of pointers.
    cout << "indirectly printing out the numbers from 0 to " 
         << N << endl;

    copy(make_indirect_iterator(pointers.begin()),
         make_indirect_iterator(pointers.end()),
         ostream_iterator<int>(cout, " "));

    cout << endl;

    return boost::exit_success;
}
