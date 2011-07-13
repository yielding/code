#include <boost/algorithm/string.hpp>
#include <iostream>
#include <cassert>

using namespace std;

namespace str 
{
using namespace boost;
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
template <typename T> 
bool is_number(T const& s)
{
    return all(s, is_digit());
}

template <typename T> 
bool is_hex(T const& s)
{
    return all(s, is_xdigit());
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
}

int main(int argc, char const* argv[])
{
    string s0("12345");
    string s1("a12345");
    string s2("a12345");

    assert(str::is_number(s0));
    assert(!str::is_number(s1));
    assert(str::is_hex(s2));
    
    return 0;
}
