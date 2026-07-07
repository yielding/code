// C++26 static reflection (P2996) — the canonical "enum <-> string" example.
//
// Key ingredients:
//   ^^X                      reflection operator: yields a std::meta::info value
//   [: r :]                  splicer: turns a reflection back into code (a value/type)
//   std::meta::enumerators_of  query the enumerators of an enum type
//   std::meta::identifier_of   get the source-level name of a reflected entity
//   template for             compile-time expansion statement over a range of info
//
// Compiler: g++-16 (experimental P2996 support)
//   g++-16 -std=c++26 -freflection main.cpp

#include <meta>

#include <print>
#include <string_view>
#include <type_traits>

using namespace std;

enum class Color { red, green, blue, yellow };

////////////////////////////////////////////////////////////////////////////////
//
// enum -> string : walk the enumerators at compile time, match the value.
//
////////////////////////////////////////////////////////////////////////////////
template <typename E>
requires is_enum_v<E>
constexpr auto enum_to_string(const E value) -> string_view
{
  template for (constexpr auto e : define_static_array(enumerators_of(^^E)))
  {
    if (value == [:e:])
      return identifier_of(e);
  }

  return "<unnamed>";
}

////////////////////////////////////////////////////////////////////////////////
//
// string -> enum : same walk, compared against each enumerator's name.
//
////////////////////////////////////////////////////////////////////////////////
template <typename E>
requires is_enum_v<E>
constexpr auto string_to_enum(const string_view name) -> E
{
  template for (constexpr auto e : define_static_array(enumerators_of(^^E)))
  {
    if (name == identifier_of(e))
      return [:e:];
  }

  return E{};
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
auto main(int argc, char* argv[]) -> int
{
  // runtime use
  println("Color::green  -> \"{}\"", enum_to_string(Color::green));
  println("\"blue\"       -> {}",    static_cast<int>(string_to_enum<Color>("blue")));

  // it is constexpr: usable at compile time
  static_assert(enum_to_string(Color::yellow) == "yellow");
  static_assert(string_to_enum<Color>("red")  == Color::red);

  return 0;
}
