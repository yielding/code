#include <iostream>

template <class D, 
class TL, class Ret = void>
class dynamic_visitor
{
  typedef typename mpl::back<TL>::type base_type;

private:  
  template<class T>
  struct visit_impl // (1)
  {
    typedef Ret (*impl_type)(T*);
    visit_impl(impl_type it = 0) : impl(it) {}
    impl_type impl;
  };

  typedef typename mpl::inherit_linearly<
    TL,   
    mpl::inherit<_1, visit_impl<_2>>
  >::type impl_map; // (2)

  template<class T>
  typename visit_impl<T>::impl_type& get_impl(visit_impl<T>& t) // (3) 
  {    
    return t.impl; 
  }

  impl_map map_;

protected:
  typedef dynamic_visitor super_t;  // (4)

  virtual Ret visit(base_type* ) {} // (5)

  template <class T>
  void assign_impl(typename visit_impl<T>::impl_type impl) // (6)
  {
    get_impl<T>(map_) = impl;
  }

public:
  Ret operator()(base_type* b)
  {
    return do_visit<TL>(b, mpl::false_());
  }

private:

  template <class T>
  Ret do_visit(base_type* b, mpl::false_)
  {
    typedef typename mpl::front<T>::type Head; 
    typedef typename mpl::pop_front<T>::type Tail;
    typedef typename mpl::empty<Tail>::type IsEmpty;

    Head* p = dynamic_cast<Head*>(b);

    if (p != 0){
      if (get_impl<Head>(map_)) { // (7)
        return get_impl<Head>(map_)(p);
      }
      return static_cast<D*>(this)->visit(p); 
    }

    return do_visit<Tail>(b, IsEmpty());<br/>  
  }

  template <class T>
  Ret do_visit(base_type* b, mpl::true_)
  {
      std::cout << "assert\n"; // handle error.

      return Ret();
  }
};

char const* get_name(D1* ) 
{ 
  return "D1"; 
}

typedef mpl::vector<D1, D2, B> TL;

struct PrintVisitor : dynamic_visitor<PrintVisitor, TL>
{
  char const* visit(B* )  { return "B";  }
  char const* visit(D2* ) { return "D2"; }
};

// usage


using namespace std;

int main(int argc, char const* argv[])
{
  PrintVisitor pv;
  cout << pv(b1) << endl;
  cout << pv(b2) << endl;<
  
  return 0;
}


