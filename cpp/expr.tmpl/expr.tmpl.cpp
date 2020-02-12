#include <vector>
#include <iostream>
#include <cassert>

using namespace std;

template <typename E>
class VecExpression 
{
public:
  double operator[](size_t i) const { return static_cast<E const&>(*this)[i]; }
  size_t size() const               { return static_cast<E const&>(*this).size(); }

  // The following overload conversions to E, the template argument type;
  // e.g., for VecExpression<VecSum>, this is a conversion to VecSum.
  operator E&()             { return static_cast<E&>(*this); }
  operator const E&() const { return static_cast<const E&>(*this); }
};

class Vec: public VecExpression<Vec> 
{
public:
  double  operator[](size_t i) const { return elems[i]; }
  double& operator[](size_t i)       { return elems[i]; }
  size_t size() const                { return elems.size(); }

  Vec(size_t n): elems(n) {}

  Vec(initializer_list<double>init)
  {
    for (auto i: init) elems.push_back(i);
  }

  // A Vec can be constructed from any VecExpression, forcing its evaluation.
  template <typename E>
  Vec(VecExpression<E> const& vec): elems(vec.size()) 
  {
    for (size_t i=0; i != vec.size(); ++i)
      elems[i] = vec[i];
  }

private:
  vector<double> elems;
};

template <typename E1, typename E2>
class VecSum : public VecExpression<VecSum<E1, E2>>
{
public:
  VecSum(E1 const& u, E2 const& v) : _u(u), _v(v)
  {
    assert(u.size() == v.size());
  }

  double operator[](size_t i) const { return _u[i] + _v[i]; }
  size_t size()               const { return _v.size();     }

private:
  E1 const& _u;
  E2 const& _v;
};

template <typename E1, typename E2>
auto operator + (E1 const& u, E2 const& v) -> const VecSum<E1, E2>
{
  return VecSum<E1, E2>(u, v);
}

int main(int argc, char *argv[]) 
{
  Vec v0 = {  23.4,  12.5, 144.56, 90.56 };
  Vec v1 = { 67.12,  34.8,  90.34, 89.30 };
  Vec v2 = { 34.90, 111.9,  45.12,  90.5 };
  Vec v3 = { 14.10,  11.8,  45.12,  90.5 };

  // Following assignment will call the ctor of Vec which accept type of 
  // `VecExpression<E> const&`. Then expand the loop body to 
  // a.elems[i] + b.elems[i] + c.elems[i] + d.elems[i]
  auto sum_of_vec_type = v0 + v1 + v2 + v3;

  for (auto i=0; i<sum_of_vec_type.size(); ++i)
    cout << sum_of_vec_type[i] << endl;

  return 0;
}
