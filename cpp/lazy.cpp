template <typename T> 
class Safe {};

template <int N> 
class Danger {
public:
  typedef char Block[N];
};

template <typename T, int N> 
class Triky 
{
public:
  virtual ~Triky() {
  }

  void no_body_here(Safe<T> = 3);

  void inclass() {
    Danger<N> no_boom_yet;
  }

  // void error() { Danger<-1> boom; }
  // void unsafe(T (*p)[N]);

  T operator->();

  struct Nested {
    Danger<N> pfew;
  };

  union {
    int align;
    Safe<T> anonymous;
  };
};

using namespace std;

int main(int argc, char const* argv[])
{
  Triky<int, -1> ok;

  return 0;
}
