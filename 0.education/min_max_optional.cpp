#include <iostream>
#include <exception>
#include <tuple>
#include <vector>
#include <optional>

using namespace std;

auto get_min_max_1(vector<int> const& v) -> tuple<int, int>
{
  if (v.empty())
    throw runtime_error("input is empty");

  auto min_ = v[0];
  auto max_ = v[0];

  for (auto const& e: v)
  {
    if (e < min_) min_ = e;
    if (e > max_) max_ = e;
  }

  return make_tuple(min_, max_);
}

auto get_min_max_2(vector<int> const& v) -> vector<int>
{
  vector<int> result;

  if (v.empty())
    return result;

  auto min_ = v[0];
  auto max_ = v[0];

  for (auto e: v)
  {
    if (e < min_) min_ = e;
    if (e > max_) max_ = e;
  }

  result.push_back(min_);
  result.push_back(max_);

  return result;
}

template<typename Container>
auto get_min_max_3(Container const& c) -> optional<tuple<int, int>>
{
  if (c.empty())
    return nullopt;

  auto min_ = *c.begin();
  auto max_ = *c.begin();

  for (auto const& e: c)
  {
    if (e < min_) min_ = e;
    if (e > max_) max_ = e;
  }

  return optional(make_tuple(min_, max_));
}

int main(int argc, char* argv[])
{
  auto v0 = vector<int>();
  auto r0 = get_min_max_3(v0);
  if (!r0.has_value())
  {
    cout << "container is empty" << endl;
  }

  auto v1 = vector{1, 2, 3, 4, 5};
  auto r1 = get_min_max_3(v1);
  if (r1)
  {
    auto [min_, max_] = r1.value();
    cout << min_ << " " << max_ << endl;
  }

  return 0;
}