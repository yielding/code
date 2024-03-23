#pragma once

#include <string>
#include <cmath>
#include <format>

////////////////////////////////////////////////////////////////////////////////
///
///
///
////////////////////////////////////////////////////////////////////////////////
struct Point
{
  Point(double x_, double y_)
    : x{x_}, y{y_}
  {}

  auto translate(double x0, double y0) const -> Point
  {
    return Point(x + x0, y + y0);
  }

  auto reversed() const -> Point
  {
    return Point(-x, -y);
  }

  auto make_rel_to(const Point& p) -> Point&
  {
    x -= p.x; y -= p.y;

    return *this;
  }

  auto rel_to(const Point& p) const -> Point
  {
    return Point(x - p.x, y - p.y);
  }

  auto dist() const -> double
  {
    return hypot(x, y);
  }

  auto dist_to(const Point& p) const -> double
  {
    return hypot(x - p.x, y - p.y);
  }

  auto area2(const Point& p0, const Point& p1) const -> double
  {
    return p0.rel_to(*this).cross(p1.rel_to(*this));
  }

  auto inner(const Point& p) const -> double
  {
    return x * p.x + y * p.y;
  }

  auto cross(const Point& p) const -> double
  {
    return x * p.y - y * p.x;
  }

  auto is_lower(const Point& p) const -> bool
  {
    return y < p.y || y == p.y && x < p.x;
  }

  auto mdist_() const -> double
  {
    return abs(x) + abs(y);
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

  auto operator == (const Point& rhs) -> bool
  {
    return x == rhs.x && y == rhs.y;
  }

  auto to_s() const -> std::string
  {
    return std::format("(x, y) = ({}, {})", x, y);
  }

  double x, y;
};

////////////////////////////////////////////////////////////////////////////////
///
///
///
////////////////////////////////////////////////////////////////////////////////
