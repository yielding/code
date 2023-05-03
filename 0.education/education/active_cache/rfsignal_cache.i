%module rfsignal_cache
#pragma SWIG nowarn=401, 801

%{
#include "rfsignal_cache.h"
%}

%include std_vector.i
%include std_string.i

namespace std {
  %template(RFSignals) vector<RFSignal>;
};


%include "rfsignal_cache.h"
