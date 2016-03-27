#include <iostream>
#include <string>
#include <vector>
#include <typeinfo>

using namespace std;

template <typename T>
class range_iterator
{
public:
  range_iterator(T init) : m_cur{init} {}

  range_iterator& operator++()
  {
    m_cur += m_step;

    return *this;
  }

  bool operator != (range_iterator<T> const& rhs) const 
  {
    return m_cur != rhs.m_cur;
  }

  T operator*() const 
  {
    return m_cur;
  }

private:
  T m_cur;
  T const m_step = 1;
};

template <typename T>
class range_impl
{
public:
  range_impl(T start, T stop) : m_start(start), m_stop(stop)
  {
  }

  auto begin() const -> range_iterator<T> 
  {
    return range_iterator<T>(m_start);
  }

  auto end() const -> range_iterator<T> 
  {
    return range_iterator<T>(m_stop);
  }

private:
  T const m_start;
  T const m_stop;
};

template <typename T>
auto range(T const start, T const stop) -> range_impl<T> 
{
  return range_impl<T>(start, stop);
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
template <typename C>
class enumerate_iterator
{
  using IndexType = size_t;
  using IterType  = decltype( begin(declval<C&>()));
  using ElemType  = decltype(*begin(declval<C&>()));
  using PairType  = pair<IndexType, ElemType>;

  IterType m_it;
  size_t m_index;

public:
  enumerate_iterator(IterType it, size_t index) :
    m_it(it), m_index(index) {}

  enumerate_iterator& operator++()
  {
    ++m_it;
    ++m_index;

    return *this;
  }

  bool operator!= (const enumerate_iterator& rhs)
  {
    return m_it != rhs.m_it;
  }

  PairType operator*() {
    return { m_index, *m_it };
  }
};

template <typename C>
class enumerate_impl
{
public:
  enumerate_impl(C&& container, size_t start)
    : m_container{forward<C>(container)}, m_start{start}
  {}

  auto begin() -> enumerate_iterator<C>
  {
    return { std::begin(m_container), m_start };
  }

  auto end() -> enumerate_iterator<C>
  {
    return { std::end(m_container), 0 };
  }

private:
  C m_container;
  const size_t m_start;
};

template <typename C>
auto enumerate(C&& container, size_t start = 0) 
  -> enumerate_impl<C>
{
  return { forward<C>(container), start };
}

template <typename T>
auto enumerate(initializer_list<T> list, size_t start = 0)
  -> enumerate_impl<vector<T>>
{
  return { vector<T>(list), start };
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////

void test1()
{
  // 1. Forward iterator를 얻을 수 있는 STL 컨테이너
  cout << "[TEST 1] vector<string>\n";
  vector<string> A = {"foo", "bar", "baz"};
  // 참조자로 직접 A 내용 수정.
  for (pair<size_t, string&> p : enumerate(A))
    cout << p.first << ": " << (p.second += p.second) << '\n';

  // 수정 내역 확인: 벡터 원소를 상수 참조자로 받음.
  for (pair<size_t, const string&> p : enumerate(A))
    cout << p.first << ": " << p.second << '\n';

  // 백터 원소를 복사해서 값으로 받음. A는 영향 없음.
  for (pair<size_t, string> p : enumerate(A))
    cout << p.first << ": " << (p.second += p.second) << '\n';

  // auto로도 받을 수 있음: p의 타입은 pair<size_t, string&>
  for (auto p : enumerate(A))
    cout << p.first << ": " << p.second << '\n';
}

void test2() {
  // 2. 일반 배열 예
  cout << "[TEST 2] array\n";
  string C[] = {"foo", "bar", "baz"};
  // auto&&로 받는 것이 범위 기반 for 문에서 일반적이고 효율적인 방법
  // p 타입: pair<size_t, string&>&&, 원소 타입이 string&로 추론
  for (auto&& p : enumerate(C, 100))
    cout << p.first << ": " << (p.second += p.second) << '\n';
  for (auto&& p : enumerate(C, 100))
    cout << p.first << ": " << p.second << '\n';
}

void test3() {
  // 3. const 예제
  cout << "[TEST 3] const\n";
  const string E[] = {"foo", "bar", "baz"};
  // decltype(p) == pair<size_t, string const&>&&
  // p 자체는 상수가 아니므로 인덱스 값은 변경 가능, 배열 값은 수정 불가.
  for (auto&& p : enumerate(E))
    cout << (p.first += 1) << ": " << p.second << '\n';
}

void test4() {
  // 4. 앞서 구현한 range 사용 예
  cout << "[TEST 4] range\n";
  auto&& D = range(100, 103);
  // decltype(p) == pair<size_t, int>&&
  for (auto&& p : enumerate(D))
    cout << p.first << ": " << p.second << '\n';
}

void test5() {
  // 5. 변수를 거치지 않고 직접 사용
  cout << "[TEST 5] in-place through rvalue reference\n";
  for (auto&& p : enumerate(range(100, 103)))
    cout << p.first << ": " << p.second << '\n';

  // decltype(p) == pair<size_t, string&>&&
  for (auto&& p : enumerate(vector<string>{"foo", "bar", "baz"}))
    cout << p.first << ": " << (p.second += p.second) << '\n';

  // 함수 반환값 바로 사용
  auto create = []()->vector<string> { return {"foo", "bar", "baz"}; };
  // decltype(p) == pair<size_t, string&>&&
  for (auto&& p : enumerate(create()))
    cout << p.first << ": " << p.second << '\n';
}

void test6() {
  // 6. 초기화 리스트
  cout << "[TEST 6] initializer list\n";
  for (auto&& p : enumerate({"foo", "bar", "baz"}))
    cout << p.first << ": " << p.second << '\n';
}

int main()
{
  test1();
  test2();
  test3();
  test4();
  test5();
  test6();
}
