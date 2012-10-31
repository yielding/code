#include "NANDUtil.h"

#include <cmath>
#include <boost/format.hpp>

using namespace boost;
using namespace std;
/////////1/////////2/////////3/////////4/////////5/////////6/////////7/////////8
//
//
//
/////////1/////////2/////////3/////////4/////////5/////////6/////////7/////////8
namespace util
{
    string sizeof_fmt(int64_t size)
    {
        char const* fmts[] = { "bytes", "KB", "MB", "GB", "TB" };
        size_t const ARR_SIZE = sizeof(fmts) / sizeof(fmts[0]);
        string res;

        for (size_t i=0; i<ARR_SIZE; i++)
        {   
            if (size < 1024.0)
            {
                res = str(format("%3.1f %s") % double(size) % fmts[i]);
                break;
            }

            size /= 1024.0;
        }   

        return res;
    }
    
    uint32_t next_power_of_two(uint32_t value)
    {
        uint32_t res = 1;
        while (res < value) 
            res <<= 1;
        
        return res;
    }
    
    // 아무리 단순해도 분리 include 하는 넘이 있으므로 
    double log2(double arg)
    {
        return log(arg) / log(2);
    }

    void vfl_checksum(void* data, int size, uint32_t* a, uint32_t* b)
    {
        uint32_t* buffer = (uint32_t*) data;
        uint32_t x = 0;
        uint32_t y = 0;
        for (int i=0; i<size/4; i++)
        {
            x += buffer[i];
            y ^= buffer[i];
        }   

        *a = x + 0xAABBCCDD;
        *b = y ^ 0xAABBCCDD;
    }

    bool vfl_check_checksum(ByteBuffer& context)
    {
        uint32_t cs1, cs2;
        vfl_checksum((uint8_t*)context, int(context.size() - 8), &cs1, &cs2);

        // Yes, I know this looks strange!! but apple use this logic
        context.offset(context.size() - 8);
        auto checksum1 = context.get_uint4_le();
        if (cs1 == checksum1)
            return true;

        auto checksum2 = context.get_uint4_le();
        if (cs2 != checksum2)
            return true;

        return false;
    }

    int ceil_divide(int value, int amount)
    {
        return (value + amount - 1) / amount;
    }
}

/////////1/////////2/////////3/////////4/////////5/////////6/////////7/////////8
//
//
//
/////////1/////////2/////////3/////////4/////////5/////////6/////////7/////////8
