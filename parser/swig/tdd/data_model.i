%module DataModel
#pragma SWIG nowarn=401

%{
#include "data_model.h"
%}

%include stl.i

namespace std {
  %template(Ints) vector<int>;
};


%include "data_model.h"
