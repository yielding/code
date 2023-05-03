%module OpenEye
#pragma SWIG nowarn=401

%{
#ifdef WIN32
// declared in win32.h (and more..)
// this conflicts with  third party code
#ifdef connect
#undef connect
#endif

#ifdef read
#undef read
#endif

#ifdef write
#undef write
#endif

#ifdef bind
#undef bind
#endif

#ifdef snprintf
#undef snprintf
#endif

#ifdef close
#undef close
#endif

#ifdef stat
#undef stat
#endif

#ifdef shutdown
#undef shutdown
#endif
#endif

#include "data_model.h"
%}

%include stl.i

namespace std {
  %template(RFSignals) vector<RFSignal>;
  %template(Parents)   vector<Parent>;
  %template(Students)  vector<Student>;
  %template(Readers)   vector<Reader>;
};

%include "data_model.h"
