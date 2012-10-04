#ifndef NANDUTIL_H
#define NANDUTIL_H

#include "NANDCore.h"

/////////1/////////2/////////3/////////4/////////5/////////6/////////7/////////8
//
// Move below functions to the appropriate code
// But, keeping all the codes together here is very important to test
//
/////////1/////////2/////////3/////////4/////////5/////////6/////////7/////////8
namespace util
{
    auto sizeof_fmt(int64_t size) -> string;

    auto next_power_of_two(uint32_t value) -> uint32_t;

    auto log2(double arg) -> double;

    void vfl_checksum(void* data, int size, uint32_t* a, uint32_t* b);
    bool vfl_check_checksum(ByteBuffer& context);
    int  ceil_divide(int value, int amount);
}

/////////1/////////2/////////3/////////4/////////5/////////6/////////7/////////8
//
//
//
/////////1/////////2/////////3/////////4/////////5/////////6/////////7/////////8
#endif
