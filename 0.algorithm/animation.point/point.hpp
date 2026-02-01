#pragma once

#include <cmath>
#include <format>
#include <string>

////////////////////////////////////////////////////////////////////////////////
///
///
///
////////////////////////////////////////////////////////////////////////////////
struct Point
{
  Point(double x, double y)
    : _x{x} , _y{y}
  {
  }

  auto translate(double x0, double y0) const -> Point
  {
    return Point(_x + x0, _y + y0);
  }

  auto reversed() const -> Point
  {
    return Point(-_x, -_y);
  }

  auto make_rel_to(const Point& p) -> Point&
  {
    _x -= p._x;
    _y -= p._y;

    return *this;
  }

  auto rel_to(const Point& p) const -> Point
  {
    return Point(_x - p._x, _y - p._y);
  }

  auto dist() const -> double
  {
    return hypot(_x, _y);
  }

  auto dist_to(const Point& p) const -> double
  {
    return hypot(_x - p._x, _y - p._y);
  }

  auto area2(const Point& p0, const Point& p1) const -> double
  {
    return p0.rel_to(*this).cross(p1.rel_to(*this));
  }

  auto inner(const Point& p) const -> double
  {
    return _x * p._x + _y * p._y;
  }

  auto cross(const Point& p) const -> double
  {
    return _x * p._y - _y * p._x;
  }

  auto is_lower(const Point& p) const -> bool
  {
    return _y < p._y || (_y == p._y && _x < p._x);
  }

  auto mdist_() const -> double
  {
    return std::abs(_x) + std::abs(_y);
  }

  auto mdist(const Point& p) const -> double
  {
    return rel_to(p).mdist_();
  }

  auto is_further(const Point& p) const -> bool
  {
    return mdist_() > p.mdist_();
  }

  auto is_between(const Point& p0, const Point& p1) const -> bool
  {
    return p0.mdist(p1) >= mdist(p0) + mdist(p1);
  }

  auto is_less(const Point& p) const -> bool
  {
    auto f = cross(p);
    return f > 0 || (f == 0 && is_further(p));
  }

  auto is_convex(const Point& p0, const Point& p1) const -> bool
  {
    auto f = area2(p0, p1);

    return f < 0 || (f == 0 && !is_between(p0, p1));
  }

  auto operator==(const Point& rhs) -> bool
  {
    return _x == rhs._x && _y == rhs._y;
  }

  auto to_s() const -> std::string
  {
    return std::format("(x, y) = ({}, {})", _x, _y);
  }

  double _x, _y;
};

////////////////////////////////////////////////////////////////////////////////
///
///
///
////////////////////////////////////////////////////////////////////////////////
