#ifndef JARVIS_MARCH_H
#define JARVIS_MARCH_H

#include "color_point.h"

#include <vector>

template <typename Color>
class JarvisMarch
{
public:
  typedef ColorPoint<Color> Point;

public:
  JarvisMarch()
  {
  }

public:
  int compute_hull(std::vector<Point> points) // copy on purpose
  {
    this->points = points;
    return calculate();
  }

private:
  int calculate()
  {
    count = 0;
    auto i = index_of_lowest_point();
    do
    {
       exchange(count, i);
       i = index_of_right_most_point_from(points[count]);
       count++;
    }
    while (i > 0);

    return count;
  }

  auto index_of_right_most_point_from(Point const& q) -> int
  {
    int i = 0;
    for (int j=1; j<points.size(); j++)
    {
      if (points[j].relative_to(q).is_less(points[i].relative_to(q)))
          i = j;
    }

    return i;
  }

  auto index_of_lowest_point() const -> int
  {
    auto min = 0;

    auto& ps = points;

    for (int i=1; i<points.size(); i++)
    {
      if (ps[min].y > ps[i].y or
         (ps[min].y == ps[i].y && ps[min].x < ps[i].x))
          min = i;
    }

    return min;
  }

  void exchange(int i, int j)
  {
      points[i].swap(points[j]);
  }

public:
  std::vector<Point> points;
  int count;
};

#endif // JARVIS_MARCH_H
