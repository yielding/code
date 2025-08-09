# Reader Monad in C++

## 개요

이 프로젝트는 C++로 구현된 **Reader Monad** 패턴입니다. Reader Monad는 함수형 프로그래밍에서 의존성 주입(dependency injection)을 우아하게 처리하는 패턴입니다.

## 핵심 구성요소

### 1. Reader 클래스
- `ReaderFunction`: 환경(`Env`)을 받아 값(`T`)을 반환하는 함수 타입
- `run(Env env)`: 환경을 제공하여 Reader를 실행
- `map(Func f)`: Functor 인터페이스 - 결과값을 변환
- `flatMap(Func f)`: Monad 인터페이스 - Reader를 체이닝

### 2. 사용 예제
```cpp
Config config = {"Hello, ", "!"};
Reader<Config, string> reader = greet("Alice");
string result = reader.run(config);  // "Hello, Alice!"
```

## 모나드(Monad) 개념

### 모나드란?

모나드는 **값을 감싸는 컨테이너**이면서 동시에 **연산을 체이닝할 수 있는 구조**입니다. 이 코드의 Reader Monad를 통해 핵심 개념을 설명하겠습니다.

### 모나드의 3가지 핵심 요소

#### 1. 타입 생성자 (Type Constructor)
```cpp
template <typename Env, typename T>
class Reader  // Reader<Env, T>가 모나드 타입
```

#### 2. return/pure (값을 모나드로 감싸기)
```cpp
Reader<Config, string>([name](Config config) {
    return config.prefix + name + config.suffix;
});
```

#### 3. bind/flatMap (모나드 체이닝)
```cpp
template <typename Func>
auto flatMap(Func f) const {
    return [=, this](Env env) { 
        return f(this->run(env)).run(env); 
    };
}
```

## 모나드 vs Functor

### Functor (map)
감싸진 값을 변환
```cpp
// string -> string 변환
auto shout = reader.map([](const string& str) { 
    return str + "!!!"; 
});
```

### Monad (flatMap)
감싸진 값을 다른 모나드로 변환
```cpp
// string -> Reader<Config, string> 변환
auto chain = reader.flatMap([](const string& result) {
    return Reader<Config, string>([result](Config cfg) {
        return "Result: " + result;
    });
});
```

## 모나드 법칙

### 1. 왼쪽 항등원 (Left Identity)
```cpp
// pure(a).flatMap(f) == f(a)
auto a = "test";
auto f = [](string s) { return greet(s); };

// 두 결과가 같음
Reader<Config, string>([a](Config c) { return a; }).flatMap(f);
f(a);
```

### 2. 오른쪽 항등원 (Right Identity)
```cpp
// m.flatMap(pure) == m
reader.flatMap([](string s) { 
    return Reader<Config, string>([s](Config c) { return s; }); 
});
// == reader
```

### 3. 결합법칙 (Associativity)
```cpp
// m.flatMap(f).flatMap(g) == m.flatMap(x => f(x).flatMap(g))
```

## 모나드의 실용적 이점

### 1. 부수효과 관리
- **Reader Monad**: 의존성 주입
- **Maybe/Optional**: null 처리
- **Either/Result**: 에러 처리
- **IO Monad**: 입출력 격리

### 2. 합성 가능성
```cpp
// 여러 Reader를 조합하여 복잡한 로직 구성
auto complexOperation = 
    greet("User")
    .map(toUpper)
    .flatMap(addTimestamp)
    .map(formatMessage);
```

### 3. 컨텍스트 전파
```cpp
// Config가 자동으로 전체 체인에 전파됨
// 명시적으로 매번 전달할 필요 없음
reader.map(f1).map(f2).flatMap(f3).run(config);
```

## 왜 모나드를 사용하는가?

1. **순수성 유지**: 부수효과를 격리하여 함수형 프로그래밍 원칙 준수
2. **에러 처리 단순화**: 에러 전파가 자동으로 처리됨
3. **테스트 용이성**: 의존성이 명확하고 주입 가능
4. **코드 가독성**: 체이닝을 통한 선언적 프로그래밍

Reader Monad는 특히 **의존성 주입이 필요한 상황**에서 유용합니다. 전역 변수나 싱글톤 대신 명시적으로 환경을 전달하면서도 깔끔한 코드를 유지할 수 있습니다.

## 작동 원리

1. **지연 실행**: Reader는 함수를 감싸고 있으며, 환경이 제공될 때까지 실행을 지연
2. **의존성 주입**: 환경(Config)을 나중에 주입하여 동일한 로직을 다른 설정으로 실행 가능
3. **함수 합성**: `map`과 `flatMap`을 통해 Reader들을 조합하여 복잡한 로직 구성

## 빌드 및 실행

### CMake 사용
```bash
mkdir build
cd build
cmake ..
make
./reader_monad
```

### 직접 컴파일
```bash
g++ -std=c++17 main.cpp -o reader_monad
./reader_monad
```

## 출력 예제
```
Hello, Alice!
Hello, Alice!!!!
```

## FAQ

### Q: Reader를 포함한 여러 모나드에 `run`이라는 인터페이스가 있는 이유는?

`run` 인터페이스는 모나드 계산의 **실행 트리거** 역할을 합니다. 이것이 필수적인 이유는 다음과 같습니다:

#### `run`의 핵심 목적

`run` 메서드는 **계산의 설명과 실행을 분리**합니다. 모나드는 무엇을 계산할지 설명만 하고, `run`이 호출될 때까지 실행하지 않습니다.

#### `run` 인터페이스가 필요한 주요 이유

1. **지연된 실행**
   - 모나드는 계산을 설명만 하고 실제 실행은 지연
   - `run`을 호출할 때 비로소 계산이 수행됨

2. **컨텍스트 제공**
   - 많은 모나드는 실행 시점에 추가 정보(컨텍스트)가 필요
   - `run`이 이 컨텍스트를 받아 계산에 제공

#### 모나드별 다양한 `run` 시그니처

```cpp
// Reader Monad - 환경이 필요
reader.run(environment);

// State Monad - 초기 상태가 필요
state.run(initialState);

// IO Monad - 부수효과 실행
io.unsafeRun();

// Writer Monad - 누적된 데이터 반환
auto [value, log] = writer.run();

// Parser Monad - 파싱할 입력이 필요
parser.runParser(input);

// Continuation Monad - 연속 함수가 필요
cont.run(continuation);
```

#### `run` 패턴의 이점

1. **지연 평가**: 실행하지 않고 복잡한 계산 구성
2. **다중 실행**: 동일한 계산을 다른 컨텍스트로 실행
3. **실행 전 합성**: 실행하기 전에 모나드들을 결합
4. **제어된 부수효과**: 효과가 발생하는 시점을 명시적으로 제어

`run` 인터페이스는 **계산 설명**과 **계산 실행**의 분리를 가능하게 하기 때문에 모나드 패턴의 핵심입니다. 이는 함수형 프로그래밍의 합성 가능성과 참조 투명성의 열쇠입니다.

### Q: Why do monads including Reader have a `run` interface?

The `run` interface serves as the **execution trigger** for monadic computations. Here's why it's essential:

#### Core Purpose of `run`

The `run` method **separates the description of a computation from its execution**. Monads describe what to compute, but don't execute until `run` is called.

#### Key Reasons for the `run` Interface

1. **Deferred Execution**
   - Monads only describe computations; actual execution is deferred
   - Computation occurs only when `run` is called

2. **Context Provision**
   - Many monads require additional information (context) at execution time
   - `run` accepts this context and provides it to the computation

#### Different Monads, Different `run` Signatures

```cpp
// Reader Monad - needs environment
reader.run(environment);

// State Monad - needs initial state
state.run(initialState);

// IO Monad - executes side effects
io.unsafeRun();

// Writer Monad - returns accumulated data
auto [value, log] = writer.run();

// Parser Monad - needs input to parse
parser.runParser(input);

// Continuation Monad - needs continuation function
cont.run(continuation);
```

#### Benefits of the `run` Pattern

1. **Lazy Evaluation**: Build complex computations without executing them
2. **Multiple Executions**: Run the same computation with different contexts
3. **Composition Before Execution**: Combine monads before running
4. **Controlled Side Effects**: Explicitly control when effects occur

The `run` interface is fundamental to the monad pattern because it enables the separation of **computation description** from **computation execution**, which is key to functional programming's composability and referential transparency.