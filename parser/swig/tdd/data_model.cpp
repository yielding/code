#include "data_model.h"

#include <boost/ref.hpp>
#include <boost/lambda/lambda.hpp>

using namespace boost::lambda;
using namespace boost;

int sum(vector<int>& data)
{
  int s = 0;
  for_each(data.begin(), data.end(), ref(s) += _1);

  return s;
}

double mean(vector<int>& data)
{
  return double(sum(data)) / data.size();
}
