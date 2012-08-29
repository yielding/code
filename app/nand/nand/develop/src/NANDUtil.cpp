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
}

/////////1/////////2/////////3/////////4/////////5/////////6/////////7/////////8
//
//
//
/////////1/////////2/////////3/////////4/////////5/////////6/////////7/////////8
