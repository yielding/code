#ifndef UNICODE_COMPARE_H_W5Y59E6U
#define UNICODE_COMPARE_H_W5Y59E6U

#include <stdint.h>
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
int32_t FastUnicodeCompare(register uint16_t const* str1, register uint16_t length1,
                           register uint16_t const* str2, register uint16_t length2);

#endif
