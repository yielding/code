%module DataModel
#pragma SWIG nowarn=401

%{
#include "signal_cache.h"
%}

%include stl.i

namespace std {
  %template(RFSignals) vector<RFSignal>;
};


%include "signal_cache.h"
