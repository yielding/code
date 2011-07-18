namespace N
{
  class A 
  {
  };

  template <typename T1> 
  T1 operator + (T1 op1, T1 op2) { return op1; }
}

template <typename T> 
class B {
};

int main(int argc, char const* argv[])
{
  N::A a1, a2;
  a1 + a2;  // ok: no doubt

  B<int> b3, b4;
  b3 + b4;  // fail: no doubt

  B<N::A> b1, b2;
  b1 + b2;  // ok : by ADL

  return 0;
}
