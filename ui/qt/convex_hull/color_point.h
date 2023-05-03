#pragma once

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
#include <string>
#include <cmath>

template <typename Color>
struct ColorPoint
{
  ColorPoint(int x, int y, Color const& c)
  {
    this->x = x;
    this->y = y;

    color = c;
  }

  auto operator == (ColorPoint const& rhs) const -> bool
  {
    return x == rhs.x && y == rhs.y;
  }

  auto relative_to(ColorPoint const& p) const -> ColorPoint
  {
    return ColorPoint(x - p.x, y - p.y, p.color);
  }

  void make_relative_to(ColorPoint const& p)
  {
    x -= p.x;
    y -= p.y;
  }

  auto translate(int x, int y) const -> ColorPoint
  {
    return ColorPoint(this->x + x, this->y + y, color);
  }

  auto reversed() const -> ColorPoint
  {
    return ColorPoint(-x, -y, color);
  }

  auto is_lower(ColorPoint const& p) const -> bool
  {
    return y < p.y || (y == p.y && x < p.x);
  }

  auto is_further(ColorPoint const& p) const -> bool
  {
    return mdist_() > p.mdist_();
  }

  auto is_less(ColorPoint const& p) const -> bool
  {
    auto f = cross_product(p);
    return f > 0 || (f == 0 and is_further(p));
  }

  auto area2(ColorPoint const& p0, ColorPoint const& p1) -> int
  {
    return p0.relative_to(*this).cross_product(p1.relative_to(*this));
  }

  auto inner_product(ColorPoint& p) const -> int
  {
    return x * p.x + y * p.y;
  }

  auto cross_product(ColorPoint const& p) const -> int
  {
    return x * p.y - y * p.x;
  }

  auto mdist_() const -> int
  {
    return std::abs(x) + std::abs(y);
  }

  auto mdist(ColorPoint& p) const -> int
  {
    return relative_to(p).mdist_();
  }

  auto located_near(int x_, int y_, int range) const -> bool
  {
    return (x > x_ - range && x < x_ + range) &&
           (y > y_ - range && y < y_ + range);
  }

  void swap(ColorPoint& other)
  {
    std::swap(x, other.x);
    std::swap(y, other.y);
    std::swap(color, other.color);
  }

  void reset()
  {
    x = y = -1;
  }

  auto to_s() -> std::string
  {
    using namespace std;

    return string("x: ") + to_string(x) + ", " +
                  "y: "  + to_string(y);
  }

  int x{};
  int y{};
  Color color;
};
