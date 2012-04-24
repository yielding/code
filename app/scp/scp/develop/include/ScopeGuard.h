#ifndef SCOPE_GUARD_H_DH70HKXS
#define SCOPE_GUARD_H_DH70HKXS

#include <boost/function.hpp>

namespace want_to_hide { namespace detail {
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
struct ScopeGuard  {
    boost::function<void(void)> _f;
    ~ScopeGuard() { _f(); }
};
}
}

#define ON_BLOCK_EXIT_INSIDE2(lambda_, num) \
    want_to_hide::detail::ScopeGuard guard##num; \
    guard##num._f = lambda_; 

#define ON_BLOCK_EXIT_INSIDE(lambda_, num) ON_BLOCK_EXIT_INSIDE2(lambda_, num) 
#define ON_BLOCK_EXIT(lambda_) ON_BLOCK_EXIT_INSIDE(lambda_, __LINE__) 

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
#endif
