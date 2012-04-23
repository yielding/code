#ifndef SCOPE_GUARD_H_DH70HKXS
#define SCOPE_GUARD_H_DH70HKXS

#include <boost/function.hpp>

namespace want_to_hide { namespace detail {
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
struct Guard  {
    boost::function<void(void)> _f;
    ~Guard() { _f(); }
};
}
}

#define ON_BLOCK_EXIT_INSIDE2(lambda_, num) \
    want_to_hide::detail::Guard guard##num; \
    guard##num._f = lambda_; 

#define ON_BLOCK_EXIT_INSIDE(lambda_, num) ON_BLOCK_EXIT_INSIDE2(lambda_, num) 
#define ON_BLOCK_EXIT(lambda_) ON_BLOCK_EXIT_INSIDE(lambda_, __LINE__) 

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
#endif
