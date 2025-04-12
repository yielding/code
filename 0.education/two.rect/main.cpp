#include <print>

using namespace std;

class Rect
{
public:
  Rect(int x0, int y0, int x1, int y1) 
    : _x0(x0), _x1(x1), _y0(y0), _y1(y1)
  {}

  auto area() const { return width() * height(); }
  auto perimeter() const { return 2 * (width() + height()); }

  auto width() const -> int { return _x1 - _x0; }
  auto height() const -> int { return _y1 - _y0; }

  auto contains(const Rect& r) const -> bool
  {
    return _x0 <= r._x0 && _x1 >= r._x1 &&
           _y0 <= r._y0 && _y1 >= r._y1;
  }

  int _x0, _x1;
  int _y0, _y1;
};

class TwoRect
{
public:
  TwoRect(const Rect& r0, const Rect& r1)
    : _r0(r0), _r1(r1) 
  {}

  auto perimeter() const -> int
  {
    if (_r0.contains(_r1)) return _r0.perimeter();
    if (_r1.contains(_r0)) return _r1.perimeter();

    auto perimeter = _r0.perimeter() + _r1.perimeter();
    return has_overlap()
      ? perimeter - 2 * (x_overlap() + y_overlap())
      : perimeter;
  }

  auto area() const -> int
  {
    if (_r0.contains(_r1)) return _r0.area();
    if (_r1.contains(_r0)) return _r1.area();

    auto area = _r0.area() + _r1.area();
    return has_overlap()
      ? area - x_overlap() * y_overlap()
      : area;
  }

  auto has_overlap() const -> bool { return _r0._x0 < _r1._x1 && _r0._y0 < _r1._y1; }
  auto x_overlap() const -> int { return min(_r0._x1, _r1._x1) - max(_r0._x0, _r1._x0); }
  auto y_overlap() const -> int { return min(_r0._y1, _r1._y1) - max(_r0._y0, _r1._y0); }

private:
  Rect _r0, _r1;
};

void println(const TwoRect& tr)
{
  println("넓이{} : 둘레 {}", tr.area(), tr.perimeter());
}

int main(int argc, char* argv[])
{
  TwoRect tr0(Rect{2, 6, 6, 8}, Rect{4, 1, 8, 5});
  println(tr0);

  TwoRect tr1(Rect{11, 1, 15, 5}, Rect{13, 3, 17, 7});
  println(tr1);

  TwoRect tr2(Rect{20, 2, 26, 6}, Rect{22, 3, 24, 5});
  println(tr2);

  return 0;
}
