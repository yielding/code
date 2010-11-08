#include <valarray>
#include <iostream>

std::ostream& operator<< (std::ostream& out, const std::valarray<int>& v)
{
  out << "[";
  for (size_t i = 0; i < v.size(); i++)
  {
    out << v[i];
    if (i < v.size()-1)
      out << ",";
  }
  out << "]";
  return out;
}
