// (C) Copyright Jeremy Siek 2000-2004.
// Distributed under the Boost Software License, Version 1.0. (See
// accompanying file LICENSE_1_0.txt or copy at
// http://www.boost.org/LICENSE_1_0.txt)

#include <boost/config.hpp>
#include <vector>
#include <iostream>
#include <iterator>
#include <functional>
#include <algorithm>
#include <boost/iterator/indirect_iterator.hpp>

using namespace std;
using namespace boost;

ostream_iterator<char> out(cout, ",");

int main(int, char*[])
{
    char characters[] = "abcdefg";
    const int N = sizeof(characters)/sizeof(char) - 1; // -1 since characters has a null char
    char* pointers_to_chars[N];                        // at the end.
    for (int i=0; i<N; ++i) pointers_to_chars[i] = &characters[i];

    // Example of using indirect_iterator
    indirect_iterator<char**, char> ifirst(pointers_to_chars), ilast(pointers_to_chars + N);

    copy(ifirst, ilast, out);
    cout << endl;

    // Example of making mutable and constant indirect iterators
    char  mutable_characters[N];
    char* pointers_to_mutable_chars[N];
    for (int j=0; j<N; ++j) pointers_to_mutable_chars[j] = &mutable_characters[j];

    indirect_iterator<char* const*> 
        mifirst(pointers_to_mutable_chars), milast(pointers_to_mutable_chars + N);
    indirect_iterator<char* const*, char const> 
        cifirst(pointers_to_chars), cilast(pointers_to_chars + N);

    transform(cifirst, cilast, mifirst, bind1st(plus<char>(), 1));

    copy(mifirst, milast, out);
    cout << endl;


    // Example of using make_indirect_iterator()
    copy(make_indirect_iterator(pointers_to_chars), 
         make_indirect_iterator(pointers_to_chars + N),
         out);
    cout << endl;

    return 0;
}
