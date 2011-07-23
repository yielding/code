#include <boost/utility/in_place_factory.hpp>
#include <boost/utility/typed_in_place_factory.hpp>

using namespace boost;

struct X
{
  X(int a) {}
  X(int a, int b) {}
  X(int a, int b, int c) {}
};

struct C
{
  C() 
    : contained_(0) 
  {}

  C(X const& v)
    : contained_(new X(v) ) 
  {}

  template<class Expr>
  C(Expr const& expr)
    : contained_(uninitialized_storage())
  {
    construct(expr,&expr);
  }

  ~C() 
  { 
    delete contained_; 
  }

  template<class InPlaceFactory>
  void construct(InPlaceFactory const& aFactory, 
      boost::in_place_factory_base const*)
  {
    aFactory.template apply<X>(contained_);
  }

  template<class TypedInPlaceFactory>
  void construct(TypedInPlaceFactory const& aFactory, 
      boost::typed_in_place_factory_base* )
  {
    aFactory.apply(contained_);
  }

  X* uninitialized_storage()
  {
    return reinterpret_cast<X*>(new char[sizeof(X)]);
  }

  X* contained_;
};


int main(int argc, char const* argv[])
{
  C a(X(1));
  C b(X(1, 2));
  C c(X(1, 2, 3));

  return 0;
}
