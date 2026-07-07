# mruby 임베딩 — C++ 호스트 사용 매뉴얼

C++ 호스트 애플리케이션에 mruby VM을 임베드하여 **스크립트 ↔ 호스트
양방향 값 교환**을 구현하는 방법.  이 저장소의 실험 시리즈(특히
`md.analyzer/`)에서 확립한 패턴과, 그 패턴을 캡슐화한 헤더 하나짜리
헬퍼 **mrubybind**를 설명한다.  forensic.filesystem의 `pfs_mruby`
프런트엔드가 이 기법의 실전 적용이다.

----

## 왜 mruby인가

mruby는 "임베딩을 위해 다시 설계된 Ruby"다.  CRuby(MRI)와 달리:

- **VM이 값이다** — 전역 인터프리터가 없다.  `mrb_open()`이 돌려주는
  `mrb_state*` 하나가 독립된 VM이고, 여러 개를 만들 수도 있다.
- **정적 링크 한 방** — `libmruby.a` 하나만 링크하면 끝.  런타임
  파일 배포, GEM_PATH, 동적 로딩이 전혀 없다.
- **기능은 빌드 타임에 고정** — 표준 라이브러리는 mrbgem 단위로
  빌드에 포함/제외한다 (`default.gembox`에는 `mruby-io`가 들어 있어
  내장 `File`/`IO`/`Dir`가 존재한다 — 아래 네임스페이스 절 참고).
- **C API가 1급 시민** — 클래스 정의, 메서드 바인딩, 예외 처리 모두
  C 함수로 제공되며 문서화가 잘 되어 있다.

포렌식 분석기 관점의 용례: 케이스마다 달라지는 추출/검증 로직을
호스트 재컴파일 없이 스크립트로 내려보내고, 호스트는 도메인 객체
(DataStore, Filesystem, File)만 노출한다.

----

## 설치와 빌드 연결

기준 환경: mruby 3.4.0 을 `~/.rubies/mruby-3.4.0`에 빌드해 둔다.

| 무엇 | 경로 |
|---|---|
| 공용 헤더 | `~/.rubies/mruby-3.4.0/include` |
| 빌드 생성 헤더 | `~/.rubies/mruby-3.4.0/build/host/include` |
| 정적 라이브러리 | `~/.rubies/mruby-3.4.0/build/host/lib/libmruby.a` |

CMake 연결은 include/lib 경로 추가 + `mruby` 링크가 전부다:

```cmake
include_directories ($ENV{HOME}/.rubies/mruby-3.4.0/include
                     $ENV{HOME}/.rubies/mruby-3.4.0/build/host/include)
link_directories    ($ENV{HOME}/.rubies/mruby-3.4.0/build/host/lib)
target_link_libraries (my_host mruby)
```

> **Linux 주의** — `libmruby.a`는 기본적으로 `-fPIC` 없이 빌드된다.
> PIE 링크가 실패하면 `add_link_options(-no-pie)`를 준다.

----

## 기본 임베딩 — 최소 골격

VM 열기 → 코드 실행 → 예외 확인 → 닫기.  이 네 줄이 전부다.

```cpp
#include <mruby.h>
#include <mruby/compile.h>

auto main() -> int
{
  mrb_state* mrb = mrb_open();
  if (!mrb) return 1;

  mrb_load_string(mrb, "puts 'hello from ruby'");
  if (mrb->exc) mrb_print_error(mrb);   // 예외는 반환값이 아니라 mrb->exc

  mrb_close(mrb);
  return 0;
}
```

여기서 바로 세 가지 규칙이 나온다.

1. **성공 판정은 `mrb->exc`로 한다.**  `mrb_load_string`의 반환값은
   "마지막 표현식의 값"이라 스크립트가 `false`로 끝나면 정상인데도
   실패로 오판한다.  예외 발생 여부만이 신뢰할 수 있는 신호다.
2. **`mrb_funcall` 후에도 반드시 `mrb->exc`를 본다.**  예외가 pending
   상태로 남은 채 반환값을 쓰면 다음 API 호출에서 이상 동작한다.
3. **처리한 예외는 `mrb->exc = nullptr`로 지운다.**  안 지우면 다음
   실행까지 오염된다.

### 파일 실행은 반드시 context를 통해

`mrb_load_string`은 에러에 파일/라인 정보가 없다.  `mrbc_context`에
파일명을 등록하면 파스 에러와 backtrace가 `script.rb:12:` 형태로
나온다.

```cpp
#include <mruby/compile.h>

auto ctx = mrb_ccontext_new(mrb);
ctx->capture_errors = TRUE;                    // 파스 에러도 예외로
mrb_ccontext_filename(mrb, ctx, "myscript.rb");
mrb_load_nstring_cxt(mrb, code.data(), code.size(), ctx);
// ... mrb->exc 확인 ...
mrb_ccontext_free(mrb, ctx);
```

### C → Ruby 호출

```cpp
auto obj = mrb_load_string(mrb, "Analyzer.new");        // 인스턴스 획득
auto v   = mrb_funcall(mrb, obj, "run", 1, mrb_int_value(mrb, 42));
if (mrb->exc) { mrb_print_error(mrb); mrb->exc = nullptr; }
```

### Ruby → C 호출 (전역 함수)

```cpp
auto host_version(mrb_state* mrb, mrb_value self) -> mrb_value
{
  return mrb_str_new_cstr(mrb, "1.0.0");
}

mrb_define_method(mrb, mrb->kernel_module, "host_version",
                  host_version, MRB_ARGS_NONE());
```

----

## C++ 객체 노출 — DATA 타입의 원리

mruby 객체에 C 포인터를 싣는 표준 메커니즘이 `MRB_TT_DATA`다.
클래스 하나를 노출하려면 다음 네 조각이 필요하다.

```cpp
// 1. 타입 태그 — 포인터의 정체와 해제 방법
static void file_free(mrb_state* mrb, void* p) { delete static_cast<file*>(p); }
static const mrb_data_type file_type = { "File", file_free };

// 2. 클래스 정의 + 인스턴스 타입 전환
auto cls = mrb_define_class(mrb, "File", mrb->object_class);
MRB_SET_INSTANCE_TT(cls, MRB_TT_DATA);      // ← 빠뜨리면 DATA_PTR가 없다

// 3. initialize — 재초기화 안전 순서
auto fi_initialize(mrb_state* mrb, mrb_value self) -> mrb_value
{
  mrb_value path;
  mrb_get_args(mrb, "S", &path);            // String 타입 체크 포함

  auto p = static_cast<file*>(DATA_PTR(self));
  if (p) file_free(mrb, p);                 // 재호출되면 이전 것 해제
  DATA_PTR(self)  = nullptr;                // ← new 중 GC가 봐도 안전하게
  DATA_TYPE(self) = nullptr;
  DATA_PTR(self)  = new file(RSTRING_PTR(path));
  DATA_TYPE(self) = &file_type;
  return self;
}

// 4. 메서드 — unwrap 후 위임
auto fi_size(mrb_state* mrb, mrb_value self) -> mrb_value
{
  auto f = static_cast<file*>(mrb_data_get_ptr(mrb, self, &file_type));
  return mrb_int_value(mrb, f->size());
}
```

핵심 규칙:

- **`MRB_SET_INSTANCE_TT(cls, MRB_TT_DATA)`는 필수.**  없으면 일반
  오브젝트가 만들어져 `DATA_PTR` 접근이 미정의 동작이다.
- **재초기화 순서** — `DATA_TYPE(self) = NULL` → `new` → `DATA_PTR`,
  `DATA_TYPE` 대입.  `new` 도중 GC가 돌면 반쯤 죽은 포인터를 free
  함수가 만지는 사고를 막는 순서다.
- **`mrb_data_get_ptr`은 타입 검사를 해 준다** — 다른 클래스 객체가
  들어오면 crash 대신 TypeError를 raise한다.

### `mrb_get_args` 포맷 지정자

`mrb_get_arg1` + raw `RSTRING_PTR` 조합 대신 포맷 지정자를 쓴다 —
지정자가 타입 검사를 해서 잘못된 인자에 TypeError를 raise하므로
호스트가 죽지 않는다.

| 지정자 | C 타입 | 의미 |
|---|---|---|
| `i` | `mrb_int` | Integer (Float이 오면 변환) |
| `f` | `mrb_float` | Float |
| `S` | `mrb_value` | String 객체 (타입 체크) |
| `z` | `const char*` | NUL 종료 C 문자열 |
| `s` | `const char*` + `mrb_int` | 포인터+길이 (바이너리 안전) |
| `b` | `mrb_bool` | truthy/falsy |
| `o` | `mrb_value` | 임의 객체 (체크 없음) |
| `A` / `A!` | `mrb_value` | Array (`!`는 nil 허용) |
| `&` | `mrb_value` | 블록 |
| `*` | `const mrb_value*` + `mrb_int` | 나머지 인자 전부 |

----

## GC 아레나 — 루프에서 컬렉션을 만들 때

mruby GC는 "아레나"에 신생 객체를 등록해 콜렉션에서 보호한다.
아레나는 **C 코드가 mruby로 돌아갈 때까지 줄어들지 않으므로**, C 루프
안에서 객체를 대량 생산하면 아레나가 계속 자라 메모리를 잡아먹고
결국 arena overflow가 난다.

패턴: 요소가 컨테이너에 **앵커된 직후** 아레나를 복원한다.

```cpp
auto hs = mrb_hash_new_capa(mrb, n);
for (auto fs : file_systems)
{
  int ai = mrb_gc_arena_save(mrb);
  auto key = mrb_str_new(mrb, ...);
  mrb_hash_set(mrb, hs, key, wrap(fs));   // hs가 key/value를 참조 → 보호됨
  mrb_gc_arena_restore(mrb, ai);          // 임시 참조만 아레나에서 해제
}
```

`hs` 자체는 스택(C 함수의 mrb_value)에 살아 있으므로 안전하고,
루프마다 만들어진 key/value는 `hs`에 매달린 순간부터 아레나의 보호가
필요 없다.

----

## 소유권 — 이 포인터는 누가 지우는가

DATA 타입의 free 함수는 클래스당 하나다.  그런데 같은 클래스의
인스턴스가 **두 가지 경로**로 만들어질 수 있다:

| 경로 | 소유자 | free 함수가 할 일 |
|---|---|---|
| 호스트가 자기 객체를 wrap해서 건네줌 | C++ (host) | 아무것도 안 함 (borrowed) |
| 스크립트가 `Klass.new`로 생성 | mruby GC | `delete` (owned) |
| 호스트가 복사본을 만들어 건네줌 | mruby GC | `delete` (owned copy) |

한 클래스에 한 free 함수만 있으므로, 소유권 정보 없이는 **borrowed를
지우거나(double free) owned를 안 지우는(leak)** 둘 중 하나가 된다.
실제로 `md.analyzer` 초기 버전은 `FileSystem` 하나의 `mrb_data_type`
으로 두 경우를 다 받다가 Ruby 생성분이 전부 누수됐다.

해법은 포인터에 소유권 플래그를 같이 싣는 것 — mrubybind의 `holder`가
정확히 이것이다 (다음 절).

----

## mrubybind — 헤더 하나로 캡슐화한 바인딩 레이어

위의 모든 패턴(소유권, 타입 체크, 아레나, 에러 리포팅)을 담은
단일 헤더 `mrubybind.hpp`.  의존성은 mruby뿐이고, 네임스페이스는
`mrubybind`, 모든 타입은 snake_case다.

| 타입 | 역할 |
|---|---|
| `vm` | RAII `mrb_state` + `mrbc_context`.  로딩/에러 리포팅 담당 |
| `klass<T>` | T를 Ruby 클래스로 정의.  ctor/method 바인딩 빌더 |
| `holder<T>` | 래핑된 포인터 + owned 플래그.  free 함수가 참조 |
| `arena_guard` | scope 기반 `mrb_gc_arena_save/restore` |
| `arg<T>` | C++ 타입 → `mrb_get_args` 지정자/변환 트레이트 |
| `to_ruby(mrb, v)` | C++ 값 → mrb_value 변환 (컨테이너 포함) |

### vm — 호스트의 진입점

```cpp
mrubybind::vm vm;
if (!vm.ok()) return 1;              // mrb_open 실패
auto mrb = vm.state();

init_data_store(mrb);                // 도메인 클래스 등록 (아래)
vm.run_file("myscript.rb");          // false면 예외가 이미 출력된 상태
```

- `run_file(path)` / `run_string(code, filename)` — context에 파일명을
  등록하고 실행하므로 에러가 파일/라인을 가진다.  성공 여부는
  `mrb->exc` 기준 (마지막 표현식 값이 아니다).
- `check()` — pending 예외가 있으면 backtrace 출력 후 클리어하고
  false 반환.  모든 진입점이 내부적으로 이걸 거친다.
- `set_global("$ds", v)` — 전역 변수 주입.  호스트 싱글턴을 스크립트에
  노출하는 표준 통로.
- `call(recv, "name", args...)` — `mrb_funcall` + 자동 `to_ruby` 인자
  변환 + `check()`.  C++에서 Ruby 메서드를 부를 때 쓴다.

### `klass<T>` — 클래스 정의 빌더

`md.analyzer`의 실제 바인딩 코드 전문:

```cpp
// file_ext.cpp — C++ 클래스 file을 MD::File로 노출
auto init_file(mrb_state* mrb) -> void
{
  mrubybind::klass<file>::define(mrb, "MD", "File")   // module, class
    .ctor<string>()                       // MD::File.new(path) — "S" 체크
    .method<&file::name>("name")
    .method<&file::size>("size")
    .method<&file::deleted>("deleted?")
    .method<&file::seek>("seek")          // 인자 지정자는 시그니처에서 유도
    .method<&file::read>("read")          // vector<uint8_t> → binary String
    .method<&file::save_to>("save_to");
}
```

- `define(mrb, "File")` — 최상위 클래스.  `define(mrb, "MD", "File")`
  — `MD` 모듈(없으면 생성) 아래 정의.  내부에서 `mrb_data_type` 세팅과
  `MRB_SET_INSTANCE_TT`까지 처리한다.
- `ctor<Args...>()` — `initialize`를 바인딩.  인자 타입 목록에서
  `mrb_get_args` 포맷 문자열을 **컴파일 타임에 생성**하므로
  `MD::File.new(123)`은 crash가 아니라 TypeError다.  재초기화 안전
  순서(DATA_TYPE 먼저 NULL)도 내장.
- `method<&T::f>("name")` — 멤버 함수 포인터에서 시그니처를 뽑아
  인자 파싱과 반환값 변환을 자동화한다.
- `method_raw("name", fn, aspec)` — 블록/해시/가변 인자처럼 자동화가
  안 되는 경우를 위한 탈출구.  `fn`은 생 `mrb_func_t`.

### holder — 소유권 플래그

모든 래핑 포인터는 `holder<T>{ptr, owned}`로 싸여 저장된다.

```cpp
// 호스트 소유(빌린 것) — free 함수는 holder만 지우고 ptr은 안 건드림
auto v = mrubybind::klass<data_store>::wrap(mrb, &data_store::instance(), false);

// 스크립트 생성(GC 소유) — ctor가 owned=true로 만들고 free가 delete
// MD::File.new("...")     ← 자동

// unwrap — 타입/초기화 검사 포함, 실패 시 raise
auto ds = mrubybind::klass<data_store>::unwrap(mrb, self);
```

같은 클래스가 borrowed와 owned 인스턴스를 동시에 가질 수 있게 되어,
위 소유권 절의 leak/double-free 딜레마가 사라진다.

### to_ruby — 반환값 변환 규칙

`method<>`가 바인딩한 함수의 반환값은 `to_ruby`가 변환한다:

| C++ 반환 | Ruby 값 |
|---|---|
| `bool` / 정수 / 부동소수 | `true/false` / Integer / Float |
| `string`, `const char*` | String |
| `vector<uint8_t>` | **binary String** (거대 Array 방지) |
| `vector<T>` | Array (요소마다 arena 복원) |
| `map<K,V>` | Hash (요소마다 arena 복원) |
| `T*` | borrowed로 wrap (호스트 소유 유지) |
| `T` (값) | 복사본을 owned로 wrap (GC가 수거) |

포인터는 빌려주고 값은 복사해 넘긴다는 규칙이 소유권 표와 정확히
대응한다.

### 전체 흐름 — md.analyzer 미니 예제

```cpp
// main.cpp
auto main(int argc, const char* argv[]) -> int
{
  mrubybind::vm vm;
  if (!vm.ok()) return 1;

  auto mrb = vm.state();
  init_data_store(mrb);     // MD::DataStore + $ds 전역
  init_file_system(mrb);    // MD::Filesystem
  init_file(mrb);           // MD::File

  return vm.run_file(argc > 1 ? argv[1] : "myscript.rb") ? 0 : 1;
}
```

```ruby
# myscript.rb
puts $ds.desc                          # 호스트 싱글턴 (borrowed)
$ds.file_systems.each { |name, fs| puts " #{name}" }

f = MD::File.new("/evidence/img.dd")   # 스크립트 생성 (GC-owned)
puts f.size
f.save_to("/tmp/out")
```

----

## 네임스페이스 — 내장 클래스와의 충돌

`default.gembox`로 빌드한 mruby에는 `mruby-io`가 포함되어 **내장
`File`/`IO`/`Dir`가 이미 존재한다**.  도메인 클래스를 최상위 `File`로
정의하면 내장 클래스를 재오픈하면서 `MRB_SET_INSTANCE_TT`가 내장
File 객체의 내부 표현까지 바꿔 VM이 깨진다.

규칙: **도메인 클래스는 전용 모듈 아래 정의한다.**
`mrubybind`는 `define(mrb, "MD", "File")` 오버로드로 이를 지원하고,
`MD::File`과 내장 `::File`은 완전히 독립적으로 공존한다.

```ruby
MD::File == File          # => false
File.exist?("/etc/hosts") # => true  (내장은 그대로)
```

----

## 함정 요약

| 함정 | 증상 | 처방 |
|---|---|---|
| 반환값으로 성공 판정 | `false`로 끝나는 정상 스크립트가 실패 처리 | `mrb->exc`로만 판정 |
| `MRB_SET_INSTANCE_TT` 누락 | `DATA_PTR` 미정의 동작, 간헐 crash | DATA 클래스 정의 직후 필수 호출 |
| 재초기화 시 이전 포인터 방치 | `initialize` 2회 호출 시 leak/UAF | DATA_TYPE=NULL → new → 대입 순서 |
| raw `RSTRING_PTR` 인자 파싱 | 잘못된 타입 인자에 호스트 crash | 포맷 지정자(`"S"`, `"i"`) 또는 `ctor<>`/`method<>` |
| 루프에서 아레나 방치 | 대량 변환 시 메모리 폭증, arena overflow | 요소 앵커 후 `arena_guard`/save·restore |
| 한 free 함수로 두 소유권 | leak 또는 double free | `holder`의 owned 플래그로 분리 |
| 최상위에 `File` 등 정의 | 내장 클래스 파괴, VM 오동작 | 전용 모듈(`MD::`) 아래 정의 |
| context 없는 로딩 | 에러에 파일/라인 없음 | `mrbc_context` + `mrb_ccontext_filename` |
| `mrb_funcall` 후 예외 미확인 | 오염된 상태로 계속 실행 | 매 호출 후 `mrb->exc` 확인·클리어 |

----

## 참고 자료

- `four.basic.cases/` — 임베딩 4단계 최소 예제
- `md.analyzer/` — 도메인 객체 노출 프로토타입 (이 문서의 예제 출처)
- `mrubybind/mrubybind.hpp` — 헬퍼 전체 소스 (~500줄)
- `mrubybind/test.cpp` — 소유권/타입체크/컬렉션 self test
- forensic.filesystem `docs/scripting.md` — 실전 mruby 프런트엔드 `pfs_mruby`
- mruby 공식: https://github.com/mruby/mruby (doc/guides/mrbgems.md, doc/api/)
