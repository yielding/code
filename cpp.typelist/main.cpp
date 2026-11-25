// typelist transform — 타입 리스트를 펼쳐(부분 특수화) 원소마다 변환(팩 확장)
//
//   1. transform<List, F>    tuple<Ts...> → tuple<F<Ts>...>  (타입 수준 map)
//   2. tagged<T>             같은 값 타입이라도 원소마다 다른 타입으로 감싸
//                            get<tagged<T>> 로 꺼내는 타입 인덱스 tuple
//   3. in_list<T, List>      리스트에 T 가 있는지 (if constexpr 로 갈래 나누기)
//
// build: cmake -B build && cmake --build build && ./build/typelist

#include <cstdint>
#include <print>
#include <string>
#include <tuple>
#include <type_traits>
#include <vector>

////////////////////////////////////////////////////////////////////////////////
//
// 1. transform — 주 템플릿은 선언만, tuple<Ts...> 모양만 정의
//
////////////////////////////////////////////////////////////////////////////////
namespace tl
{
  using namespace std;

  template <typename List, template <typename> class F>
  struct transform;                        // tuple 이 아니면 정의가 없어 컴파일 오류

  template <typename... Ts, template <typename> class F>
  struct transform<tuple<Ts...>, F>        // tuple 을 풀어 Ts... 를 꺼냄
  {
    using type = tuple<F<Ts>...>;          // 원소마다 F 를 적용해 다시 모음
  };

  template <typename List, template <typename> class F>
  using transform_t = typename transform<List, F>::type;

  // 3. in_list — 팩 확장 + fold 로 "하나라도 같은가"
  template <typename T, typename List>
  inline constexpr auto in_list = false;

  template <typename T, typename... Ts>
  inline constexpr auto in_list<T, tuple<Ts...>> = (is_same_v<T, Ts> or ...);
}

////////////////////////////////////////////////////////////////////////////////
//
// 2. tagged — 원소 타입이 모두 같아도 T 마다 다른 타입이 되게 감싼다
//
////////////////////////////////////////////////////////////////////////////////
namespace demo
{
  using namespace std;

  struct user { string name; };
  struct order { int64_t id; };
  struct invoice { double amount; };

  // 표마다 같은 모양의 상태(예: 준비된 SQL 문, 행 개수)
  struct table_state
  {
    string sql;
    int rows = 0;
  };

  template <typename Row>
  struct tagged
  {
    table_state state;
  };

  template <typename T>
  using vector_of = vector<T>;

  using rows = tuple<user, order, invoice>;

  // tuple<tagged<user>, tagged<order>, tagged<invoice>>
  using states = tl::transform_t<rows, tagged>;

  class store
  {
  public:
    // 타입으로 인덱싱: 런타임 조회 없이 컴파일 시간에 해당 원소를 찾는다
    template <typename Row>
    auto& state()
    {
      static_assert(tl::in_list<Row, rows>, "a row this store has no table for");
      return get<tagged<Row>>(_states).state;
    }

    template <typename Row>
    auto put(Row const&) -> void
    {
      ++state<Row>().rows;
    }

  private:
    states _states;
  };
}

////////////////////////////////////////////////////////////////////////////////
//
// 컴파일 시간 확인
//
////////////////////////////////////////////////////////////////////////////////
namespace
{
  using namespace std;
  using namespace demo;

  static_assert(is_same_v<tl::transform_t<tuple<int, char>, add_pointer_t>,
                          tuple<int*, char*>>);
  static_assert(is_same_v<tl::transform_t<rows, vector_of>,
                          tuple<vector<user>, vector<order>, vector<invoice>>>);
  static_assert(is_same_v<states,
                          tuple<tagged<user>, tagged<order>, tagged<invoice>>>);
  static_assert(tl::in_list<order, rows>);
  static_assert(!tl::in_list<string, rows>);

  // 같은 table_state 를 그대로 담으면 get<table_state> 는 모호해서 쓸 수 없다:
  //   auto t = tuple<table_state, table_state>{};
  //   get<table_state>(t);   // error: 같은 타입이 둘
}

int main()
{
  auto s = demo::store{};
  s.state<demo::user>().sql = "INSERT INTO user VALUES(?)";
  s.state<demo::order>().sql = "INSERT INTO order VALUES(?)";

  s.put(demo::user{"kim"});
  s.put(demo::user{"lee"});
  s.put(demo::order{42});

  std::println("user    {} row(s)  {}", s.state<demo::user>().rows,
    s.state<demo::user>().sql);
  std::println("order   {} row(s)  {}", s.state<demo::order>().rows,
    s.state<demo::order>().sql);
  std::println("invoice {} row(s)", s.state<demo::invoice>().rows);

  // s.put(std::string{});   // static_assert: a row this store has no table for

  return 0;
}
