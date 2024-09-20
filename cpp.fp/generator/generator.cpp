// Notice
// http://cpptruths.blogspot.kr/2015/06/fun-with-lambdas-c14-style-part-4.html
#include <iostream>
#include <memory>
#include <random>
#include <string>
#include <vector>
#include <utility>

using namespace std;

struct RootRandomGen
{
  long int operator()() const
  {
    return random();
  }
};

template <typename GenFunc>
auto make_gen_from(GenFunc&& func);

template <class T, class GenFunc>
class Gen
{
public:
  explicit Gen(GenFunc func): genfunc(move(func)) 
  {} 
   
  T generate() 
  {   
    return genfunc();
  }  

  template <class Func>
  auto map(Func&& func)
  {
    return make_gen_from(
      [gt=*this, func=std::forward<Func>(func)]() mutable { 
        return func(gt.generate()); 
      }
    );
  }

  template <class UGen, class Zipper2>
  auto zip2(UGen&& ugen, Zipper2&& func)
  {
    return this->map(
      [ugen = forward<UGen>(ugen),
       func = forward<Zipper2>(func)](auto&& t) mutable {
         return func(forward<decltype(t)>(t), ugen.generate());
      }
    );
  }

private:
  GenFunc genfunc;
};

template <typename GenFunc>
auto make_gen_from(GenFunc&& func)
{
  return Gen<decltype(func()), GenFunc>(forward<GenFunc>(func));
}

template <typename T>
auto make_gen();

template<>
auto make_gen<long int>()
{
  return make_gen_from(RootRandomGen());
}

template<>
auto make_gen<int>()
{
  return make_gen<long int>().map(
            [](long int i) { return (int)(i); });
}

template <typename Integer>
auto make_range_gen(Integer lo, Integer hi)
{
  return make_gen<long int>().map(
    [lo, hi](long int x) { return static_cast<Integer>(lo + x % (hi - lo)); }
  );
}

/*
template <typename Gen, class Func>
auto map(Gen gt, Func func)
{
    return make_gen_from([gt, func]() { 
            return func(gt.generate()); 
            });
}
*/

template <class Gen, class Func>
auto map(Gen&& gt, Func&& func)
{
  return make_gen_from(
    [gt=forward<Gen>(gt), func=forward<Func>(func)]() mutable 
    { 
      return func(gt.generate()); 
    }
  );
}

auto fiboGen()
{
  int a = 0, b = 1;

  return make_gen_from(
    [a, b]() mutable {
      int c = a; a = b; b = c + b;
      return c;
    }
  );
}

int main(int argc, char *argv[])
{
  time_t t; time(&t);
  srandom(t);

  auto gen = make_gen<long int>();

  // for (int i=0; i<3; i++) cout << gen.generate() << endl;

  auto boolgen = map(gen, [](long int i) {return bool(i%2); });
  cout << boolalpha << boolgen.generate();
  cout << endl;

  auto gen2 = make_gen<long int>();
  for (int i=0; i<3; i++) cout << gen2.generate() << endl;

  auto upper_gen = make_range_gen<char>('A', 'Z' + 1);
  cout << upper_gen.generate();

  auto uppergen = make_range_gen<char>('A', 'Z' + 1);
  auto lowergen = make_range_gen<char>('a', 'z' + 1);
  auto pairgen  = uppergen.zip2(lowergen, 
      [](char up, char low) { return make_pair(up, low); });

  cout << pairgen.generate().first;
  cout << pairgen.generate().second;

  return 0;
}
