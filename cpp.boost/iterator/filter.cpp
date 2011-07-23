#include <boost/iterator/filter_iterator.hpp>
#include <boost/cstdlib.hpp> // for exit_success
#include <algorithm>
#include <functional>
#include <iostream>

using namespace std;
using namespace boost;

struct is_positive_number {
    bool operator()(int x) { return 0 < x; }
};

int main()
{
    int numbers_[] = { 0, -1, 4, -3, 5, 8, -2 };
    const int N = sizeof(numbers_)/sizeof(int);

    typedef int* base_iterator;
    base_iterator numbers(numbers_);

    // Example using make_filter_iterator()
    copy(make_filter_iterator<is_positive_number>(numbers, numbers+N),
         make_filter_iterator<is_positive_number>(numbers+N, numbers+N),
         ostream_iterator<int>(cout, " "));
    cout << endl;

    // Example using filter_iterator
    typedef filter_iterator<is_positive_number, base_iterator>
        FilterIter;

    is_positive_number predicate;
    FilterIter filter_iter_first(predicate, numbers, numbers+N);
    FilterIter filter_iter_last (predicate, numbers+N, numbers+N);

    copy(filter_iter_first, filter_iter_last, ostream_iterator<int>(cout, " "));
    cout << endl;

    // Another example using make_filter_iterator()
    copy(make_filter_iterator(bind2nd(greater<int>(), -2), numbers, numbers+N) , 
         make_filter_iterator(bind2nd(greater<int>(), -2), numbers+N, numbers+N) , 
         ostream_iterator<int>(cout, " ")
    );

    cout << endl;

    return boost::exit_success;
}
