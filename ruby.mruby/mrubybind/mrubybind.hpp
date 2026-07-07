#pragma once

#include <mruby.h>
#include <mruby/array.h>
#include <mruby/class.h>
#include <mruby/compile.h>
#include <mruby/data.h>
#include <mruby/hash.h>
#include <mruby/string.h>
#include <mruby/variable.h>

#include <cstdint>
#include <fstream>
#include <iostream>
#include <map>
#include <sstream>
#include <string>
#include <tuple>
#include <type_traits>
#include <utility>
#include <vector>

////////////////////////////////////////////////////////////////////////////////
//
// mrubybind — a thin helper for embedding mruby in a C++ host.
//
// Encapsulates the patterns this repository learned the hard way:
//   - ownership: every wrapped pointer carries an owned/borrowed flag,
//     so one class can hand out host-owned pointers and GC-owned copies
//     without leaking or double-freeing.
//   - type safety: constructor/method arguments are parsed with
//     mrb_get_args format specifiers generated from the C++ signature,
//     so a bad script argument raises TypeError instead of crashing.
//   - GC arena: collection conversion restores the arena per element.
//   - errors: scripts run through an mrb_ccontext with a filename, and
//     every entry point reports exceptions with a backtrace.
//
////////////////////////////////////////////////////////////////////////////////
namespace mrubybind
{
  using namespace std;

  template <typename T> class Klass;

  ////////////////////////////////////////////////////////////////////////////
  //
  // GC arena guard
  //
  ////////////////////////////////////////////////////////////////////////////
  class ArenaGuard
  {
  public:
    ArenaGuard(mrb_state* mrb)
      : _mrb(mrb)
      , _index(mrb_gc_arena_save(mrb))
    {
    }

    ~ArenaGuard()
    {
      mrb_gc_arena_restore(_mrb, _index);
    }

    ArenaGuard(const ArenaGuard&) = delete;
    auto operator=(const ArenaGuard&) -> ArenaGuard& = delete;

  private:
    mrb_state* _mrb;
    int _index;
  };

  ////////////////////////////////////////////////////////////////////////////
  //
  // argument extraction — one trait per C++ parameter type.
  // spec feeds mrb_get_args, which type-checks and raises TypeError.
  //
  ////////////////////////////////////////////////////////////////////////////
  template <typename T, typename Enable = void> struct Arg;

  template <typename T>
  struct Arg<T, enable_if_t<is_integral_v<T> && !is_same_v<T, bool>>>
  {
    using storage = mrb_int;
    static constexpr char spec = 'i';
    static auto convert(mrb_state*, const storage v) -> T { return static_cast<T>(v); }
  };

  template <typename T>
  struct Arg<T, enable_if_t<is_floating_point_v<T>>>
  {
    using storage = mrb_float;
    static constexpr char spec = 'f';
    static auto convert(mrb_state*, const storage v) -> T { return static_cast<T>(v); }
  };

  template <>
  struct Arg<bool>
  {
    using storage = mrb_bool;
    static constexpr char spec = 'b';
    static auto convert(mrb_state*, const storage v) -> bool { return v != 0; }
  };

  template <>
  struct Arg<string>
  {
    using storage = mrb_value;
    static constexpr char spec = 'S';
    static auto convert(mrb_state*, const storage v) -> string
    {
      return string(RSTRING_PTR(v), RSTRING_LEN(v));  // NUL-safe
    }
  };

  template <typename T>
  struct Arg<T*>
  {
    using storage = mrb_value;
    static constexpr char spec = 'o';
    static auto convert(mrb_state* mrb, const storage v) -> T*
    {
      return Klass<T>::unwrap(mrb, v);
    }
  };

  template <>
  struct Arg<mrb_value>
  {
    using storage = mrb_value;
    static constexpr char spec = 'o';
    static auto convert(mrb_state*, const storage v) -> mrb_value { return v; }
  };

  template <typename... A>
  auto get_args(mrb_state* mrb) -> tuple<decay_t<A>...>
  {
    if constexpr (sizeof...(A) == 0)
    {
      return {};
    }
    else
    {
      tuple<typename Arg<decay_t<A>>::storage...> st{};
      constexpr char fmt[] = { Arg<decay_t<A>>::spec..., '\0' };
      apply([&](auto&... s) { mrb_get_args(mrb, fmt, &s...); }, st);

      return [&]<size_t... I>(index_sequence<I...>) {
        return tuple<decay_t<A>...>{ Arg<decay_t<A>>::convert(mrb, get<I>(st))... };
      }(index_sequence_for<A...>{});
    }
  }

  ////////////////////////////////////////////////////////////////////////////
  //
  // value conversion C++ -> Ruby
  //
  ////////////////////////////////////////////////////////////////////////////
  template <typename T> struct is_vector : false_type {};
  template <typename E, typename A> struct is_vector<vector<E, A>> : true_type {};

  template <typename T> struct is_map : false_type {};
  template <typename K, typename V, typename C, typename A>
  struct is_map<map<K, V, C, A>> : true_type {};

  template <typename V>
  auto to_ruby(mrb_state* mrb, V&& v) -> mrb_value
  {
    using U = remove_cvref_t<V>;

    if constexpr (is_same_v<U, mrb_value>)
    {
      return v;
    }
    else if constexpr (is_same_v<U, bool>)
    {
      return mrb_bool_value(v);
    }
    else if constexpr (is_integral_v<U>)
    {
      return mrb_int_value(mrb, static_cast<mrb_int>(v));
    }
    else if constexpr (is_floating_point_v<U>)
    {
      return mrb_float_value(mrb, static_cast<mrb_float>(v));
    }
    else if constexpr (is_same_v<U, string>)
    {
      return mrb_str_new(mrb, v.data(), static_cast<mrb_int>(v.size()));
    }
    else if constexpr (is_same_v<U, const char*> || is_same_v<U, char*>)
    {
      return mrb_str_new_cstr(mrb, v);
    }
    else if constexpr (is_pointer_v<U>)
    {
      // a raw pointer stays host-owned: wrap it borrowed
      return Klass<remove_pointer_t<U>>::wrap(mrb, v, false);
    }
    else if constexpr (is_vector<U>::value)
    {
      if constexpr (is_same_v<typename U::value_type, uint8_t>)
      {
        // byte buffers become a binary Ruby string, not a huge Array
        return mrb_str_new(mrb, reinterpret_cast<const char*>(v.data()),
                           static_cast<mrb_int>(v.size()));
      }
      else
      {
        auto ar = mrb_ary_new_capa(mrb, static_cast<mrb_int>(v.size()));
        for (auto& e : v)
        {
          ArenaGuard guard(mrb);
          mrb_ary_push(mrb, ar, to_ruby(mrb, e));
        }

        return ar;
      }
    }
    else if constexpr (is_map<U>::value)
    {
      auto hs = mrb_hash_new_capa(mrb, static_cast<mrb_int>(v.size()));
      for (auto& [key, val] : v)
      {
        ArenaGuard guard(mrb);
        mrb_hash_set(mrb, hs, to_ruby(mrb, key), to_ruby(mrb, val));
      }

      return hs;
    }
    else
    {
      // value-like object: copy it and hand ownership to the GC
      return Klass<U>::wrap(mrb, new U(std::forward<V>(v)), true);
    }
  }

  ////////////////////////////////////////////////////////////////////////////
  //
  // member function signature introspection
  //
  ////////////////////////////////////////////////////////////////////////////
  template <typename M> struct method_traits;

  template <typename R, typename C, typename... A>
  struct method_traits<R (C::*)(A...)>
  {
    using ret  = R;
    using cls  = C;
    using args = tuple<decay_t<A>...>;
    static constexpr size_t arity = sizeof...(A);
  };

  template <typename R, typename C, typename... A>
  struct method_traits<R (C::*)(A...) const> : method_traits<R (C::*)(A...)> {};

  template <auto Method, typename T, typename ArgsTuple> struct invoker;

  template <auto Method, typename T, typename... A>
  struct invoker<Method, T, tuple<A...>>
  {
    static auto call(mrb_state* mrb, mrb_value self, T* obj) -> mrb_value
    {
      auto args = get_args<A...>(mrb);
      using R = typename method_traits<decltype(Method)>::ret;

      if constexpr (is_void_v<R>)
      {
        apply([obj](auto&&... a) { (obj->*Method)(static_cast<decltype(a)&&>(a)...); },
              std::move(args));

        return self;
      }
      else
      {
        decltype(auto) r = apply(
          [obj](auto&&... a) -> decltype(auto) {
            return (obj->*Method)(static_cast<decltype(a)&&>(a)...);
          },
          std::move(args));

        return to_ruby(mrb, std::forward<decltype(r)>(r));
      }
    }
  };

  ////////////////////////////////////////////////////////////////////////////
  //
  // ownership-aware DATA class binding
  //
  ////////////////////////////////////////////////////////////////////////////
  template <typename T>
  struct Holder
  {
    T* ptr;
    bool owned;
  };

  template <typename T>
  class Klass
  {
  public:
    static auto define(mrb_state* mrb, const string name) -> Klass
    {
      _name = name;
      _type.struct_name = _name.c_str();
      _type.dfree = free_holder;
      _cls = mrb_define_class(mrb, _name.c_str(), mrb->object_class);
      MRB_SET_INSTANCE_TT(_cls, MRB_TT_DATA);

      return Klass(mrb);
    }

  public:
    template <typename... Args>
    auto ctor() -> Klass&
    {
      mrb_define_method(_mrb, _cls, "initialize", init_thunk<Args...>,
                        MRB_ARGS_REQ(sizeof...(Args)));
      return *this;
    }

    template <auto Method>
    auto method(const string name) -> Klass&
    {
      using traits = method_traits<decltype(Method)>;
      mrb_define_method(_mrb, _cls, name.c_str(), method_thunk<Method>,
                        MRB_ARGS_REQ(traits::arity));
      return *this;
    }

    // escape hatch for blocks, hashes, variadics, ...
    auto method_raw(const string name, mrb_func_t fn, const mrb_aspec aspec) -> Klass&
    {
      mrb_define_method(_mrb, _cls, name.c_str(), fn, aspec);
      return *this;
    }

    static auto wrap(mrb_state* mrb, T* p, const bool owned) -> mrb_value
    {
      auto holder = new Holder<T>{p, owned};
      return mrb_obj_value(Data_Wrap_Struct(mrb, _cls, &_type, holder));
    }

    static auto unwrap(mrb_state* mrb, mrb_value obj) -> T*
    {
      // raises TypeError on foreign or uninitialized data
      auto holder = static_cast<Holder<T>*>(mrb_data_get_ptr(mrb, obj, &_type));
      if (holder == nullptr || holder->ptr == nullptr)
        mrb_raisef(mrb, E_RUNTIME_ERROR, "uninitialized %s", _name.c_str());

      return holder->ptr;
    }

  private:
    Klass(mrb_state* mrb)
      : _mrb(mrb)
    {
    }

    static void free_holder(mrb_state*, void* p)
    {
      auto holder = static_cast<Holder<T>*>(p);
      if (holder == nullptr)
        return;

      if (holder->owned)
        delete holder->ptr;

      delete holder;
    }

    template <typename... Args>
    static auto init_thunk(mrb_state* mrb, mrb_value self) -> mrb_value
    {
      // safe re-initialization: detach the old payload before allocating,
      // so a GC triggered by `new` cannot touch a half-dead pointer
      if (auto old = static_cast<Holder<T>*>(DATA_PTR(self)))
        free_holder(mrb, old);

      DATA_PTR(self)  = nullptr;
      DATA_TYPE(self) = nullptr;

      auto args = get_args<Args...>(mrb);
      auto obj  = apply(
        [](auto&&... a) { return new T(static_cast<decltype(a)&&>(a)...); },
        std::move(args));

      DATA_PTR(self)  = new Holder<T>{obj, true};
      DATA_TYPE(self) = &_type;

      return self;
    }

    template <auto Method>
    static auto method_thunk(mrb_state* mrb, mrb_value self) -> mrb_value
    {
      using traits = method_traits<decltype(Method)>;
      auto obj = unwrap(mrb, self);

      return invoker<Method, T, typename traits::args>::call(mrb, self, obj);
    }

  private:
    mrb_state* _mrb;

    static inline RClass* _cls = nullptr;
    static inline string _name;
    static inline mrb_data_type _type{};
  };

  ////////////////////////////////////////////////////////////////////////////
  //
  // VM — RAII mrb_state with context-based loading and error reporting
  //
  ////////////////////////////////////////////////////////////////////////////
  class VM
  {
  public:
    VM()
      : _mrb(mrb_open())
      , _ctx(nullptr)
    {
      if (_mrb != nullptr)
      {
        _ctx = mrb_ccontext_new(_mrb);
        _ctx->capture_errors = TRUE;  // parse errors arrive as exceptions
      }
    }

    ~VM()
    {
      if (_ctx != nullptr)
        mrb_ccontext_free(_mrb, _ctx);

      if (_mrb != nullptr)
        mrb_close(_mrb);
    }

    VM(const VM&) = delete;
    auto operator=(const VM&) -> VM& = delete;

  public:
    auto ok() const -> bool { return _mrb != nullptr; }
    auto state() const -> mrb_state* { return _mrb; }

    auto run_file(const string path) -> bool
    {
      ifstream ifs(path, ios::binary);
      if (!ifs)
      {
        cerr << "mrubybind: cannot open script: " << path << "\n";
        return false;
      }

      stringstream ss;
      ss << ifs.rdbuf();

      return run_string(ss.str(), path);
    }

    auto run_string(const string code, const string filename = "(inline)") -> bool
    {
      mrb_ccontext_filename(_mrb, _ctx, filename.c_str());
      mrb_load_nstring_cxt(_mrb, code.data(), code.size(), _ctx);

      return check();
    }

    // report (with backtrace) and clear a pending exception; true when none
    auto check() -> bool
    {
      if (_mrb->exc == nullptr)
        return true;

      mrb_print_error(_mrb);
      _mrb->exc = nullptr;

      return false;
    }

    auto set_global(const string name, mrb_value v) -> void
    {
      mrb_gv_set(_mrb, mrb_intern_cstr(_mrb, name.c_str()), v);
    }

    template <typename... Args>
    auto call(mrb_value recv, const string name, Args&&... args) -> mrb_value
    {
      auto v = mrb_funcall(_mrb, recv, name.c_str(), sizeof...(Args),
                           to_ruby(_mrb, std::forward<Args>(args))...);

      return check() ? v : mrb_nil_value();
    }

  private:
    mrb_state* _mrb;
    mrb_ccontext* _ctx;
  };
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
