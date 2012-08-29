#ifndef NANDUTIL_H
#define NANDUTIL_H

#include <stdint.h>
#include <string>

/////////1/////////2/////////3/////////4/////////5/////////6/////////7/////////8
//
// move below functions to the appropriate code
// But, keeping all the codes together here is very important to test
//
/////////1/////////2/////////3/////////4/////////5/////////6/////////7/////////8
using std::string;

namespace util
{
    auto sizeof_fmt(int64_t size) -> string;

    auto next_power_of_two(uint32_t value) -> uint32_t;

    auto log2(double arg) -> double;
}

/////////1/////////2/////////3/////////4/////////5/////////6/////////7/////////8
//
//
//
/////////1/////////2/////////3/////////4/////////5/////////6/////////7/////////8
#endif
