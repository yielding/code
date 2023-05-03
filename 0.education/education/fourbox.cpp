#include <cassert>
#include <iostream>
#include <set>
#include <string>
#include <vector>
#include <tuple>
#include <sstream>

using namespace std;

struct coord
{
  coord (int x_, int y_) : x(x_), y(y_) 
  {}

  bool operator<(coord const& rhs) const
  {
    if (x > rhs.x)  return false;
    if (x == rhs.x) return y < rhs.y;

    return true;
  }

  int x, y;
};

class FourBox
{
public:
  typedef std::tuple<int, int, int, int> rect_t;

public:
  FourBox(string in)
  {
    stringstream ss(in);
    for (int i=0; i<4; i++)
    {
      int x1, y1, x2, y2; ss >> x1 >> y1 >> x2 >> y2;

      _rects.push_back(make_tuple(x1, y1, x2, y2));
    }
  }

  uint32_t area()
  {
    set<coord> area;
    for (auto it=_rects.begin(); it!=_rects.end(); ++it)
    {
      int x1, y1, x2, y2; tie(x1, y1, x2, y2) = *it;

      for (auto x=x1; x<x2; ++x)
        for (auto y=y1; y<y2; ++y) 
          area.insert(coord(x, y));
    }

    return area.size();
  }

private:
  vector<rect_t> _rects;
};

int main(int argc, const char *argv[])
{
  string in1 = "0 0 200 200    0 0 200 200    0 0 200 200    0 0 200 200";
  FourBox boxes1(in1);
  assert(boxes1.area() == 40000);

  string in2 = "1 2 4 4    2 3 5 7    3 1 6 5     7 3 8 6";
  FourBox boxes2(in2);
  assert(boxes2.area() == 26);

  return 0;
}
