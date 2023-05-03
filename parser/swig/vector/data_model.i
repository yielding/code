%module DataModel

%{
#include "data_model.h"
%}

%include std_vector.i

namespace std {
  %template(Ints) vector<int>;
}

%include "data_model.h"

# vi:filetype=swig
