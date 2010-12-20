#ifdef SUMMER_EXPORTS
#define SUMMER_API __declspec(dllexport)
#else
#define SUMMER_API __declspec(dllimport)
#endif

#include "sum.h"

#include <vector>

// This class is exported from the summer.dll
class SUMMER_API CSummer {
public:
	CSummer();
  virtual double sum(std::vector<double>& data);
  virtual double sum2(Sum& data);
};

typedef CSummer* create_summer_t();
