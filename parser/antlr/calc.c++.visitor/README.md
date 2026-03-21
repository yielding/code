# LabeledExpr Calculator (C++ Visitor)

## ANTLR란?

ANTLR(ANother Tool for Language Recognition)는 Terence Parr가 개발한 파서 생성기(parser generator)로, 문법(grammar) 파일(`.g4`)을 입력받아 해당 언어를 인식하는 lexer와 parser 소스 코드를 자동으로 생성해준다.

### 주요 특징

- **LL(*) 파싱** — 강력한 적응형 LL 파싱 알고리즘(ALL(*))을 사용하여 대부분의 문맥 자유 문법을 처리
- **다중 타겟 언어** — 하나의 `.g4` 문법으로 Java, C++, Python, JavaScript, Go, C# 등 다양한 언어의 코드를 생성
- **두 가지 트리 순회 패턴** 지원:
  - **Listener** — 이벤트 기반, parse tree를 자동 순회하며 콜백 호출 (SAX 방식과 유사)
  - **Visitor** — 명시적 순회, 각 노드 방문 시 값을 반환할 수 있어 계산기 등에 적합
- **풍부한 생태계** — ANTLRWorks(IDE), 문법 저장소(grammars-v4), 다양한 플러그인 제공

### 워크플로우

```
문법 정의(.g4) → ANTLR4 실행 → Lexer/Parser 코드 생성 → 사용자 코드에서 활용
```

## C++ 코드 생성 방법

`LabeledExpr.g4` 문법 파일로부터 ANTLR4를 사용하여 C++ 코드를 생성:

```bash
antlr4 -no-listener -visitor -Dlanguage=Cpp LabeledExpr.g4
```

### 옵션 설명

- `-no-listener` — Listener 패턴 코드를 생성하지 않음
- `-visitor` — Visitor 패턴 코드를 생성함
- `-Dlanguage=Cpp` — C++ 타겟으로 코드 생성

### 자동 생성 파일

| 파일 | 역할 |
|---|---|
| `LabeledExprLexer.cpp/h` | 어휘 분석기 (토큰화) |
| `LabeledExprParser.cpp/h` | 구문 분석기 (파싱) |
| `LabeledExprVisitor.cpp/h` | Visitor 인터페이스 |
| `LabeledExprBaseVisitor.cpp/h` | Visitor 기본 구현체 |
| `*.interp`, `*.tokens` | ANTLR 내부 메타데이터 |

### 직접 작성 파일

- `main.cpp` — 프로그램 진입점
- `EvalVisitor.h` — BaseVisitor를 상속하여 실제 계산 로직 구현
- `t.expr` — 테스트 입력 파일

## 빌드

```bash
cmake -B build && cmake --build build
./build/calc t.expr
```
