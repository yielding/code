#include <iostream>
#include <functional>
#include <sstream>

using namespace std; 

auto make_fibo()
{
  return [](int n) {
    function<int(int)> recurse;
    recurse = [&](int n) {
      return n<=2 ? 1 : recurse(n-1) + recurse(n-2);
    };

    return recurse(n);
  };
}

auto from_to = [](auto start, auto finish) {
  return [=]() mutable {
    if (start < finish)
      return start++;
    else
      throw runtime_error("complete");
  };
};

auto unit = [](auto x) {
  return [=] { return x; };
};

auto bind_ = [](auto u) {
  return [=](auto callback) {
    return callback(u());
  };
};

auto stringify = [](auto x) {
  stringstream ss;
  ss << x;
  return unit(ss.str());
};

int main(int argc, char *argv[])
{
  auto fibo = make_fibo();

  cout << fibo(10) << endl;

  auto range = from_to(0, 10);
  cout << range() << endl;

  cout << stringify(5)()
       << "=="
       << bind_(stringify(5))(unit)()
       << endl;
  
  return 0;
}
