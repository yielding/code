# gRPC C++ Hello World Example

You can find a complete set of instructions for building gRPC and running the
Hello World app in the [C++ Quick Start][].

[C++ Quick Start]: https://grpc.io/docs/languages/cpp/quickstart

## CMakeLists.txt 분석

### 빌드 설정

| 항목 | 값 |
|------|-----|
| CMake 최소 버전 | 3.14 |
| C++ 표준 | C++17 |
| 컴파일 명령 DB | `compile_commands.json` 자동 생성 (`CMAKE_EXPORT_COMPILE_COMMANDS ON`) |

### 외부 라이브러리

모든 라이브러리는 **CMake CONFIG 모드**(`find_package(... CONFIG)`)로 탐색하며, 시스템에 사전 설치되어 있어야 한다.

| 라이브러리 | 패키지명 | 링크 타겟 | 출처 | 용도 |
|-----------|----------|----------|------|------|
| **gRPC** | `gRPC` | `gRPC::grpc++`, `gRPC::grpc++_reflection` | [grpc/grpc](https://github.com/grpc/grpc) | RPC 프레임워크 핵심 라이브러리. `grpc++_reflection`은 서버가 자신의 서비스 정의를 런타임에 노출할 수 있게 해준다 (grpc_cli 등 디버깅 도구에서 활용). |
| **Protobuf** | `Protobuf` | `protobuf::libprotobuf` | [protocolbuffers/protobuf](https://github.com/protocolbuffers/protobuf) | `.proto` 파일에서 정의한 메시지의 직렬화/역직렬화를 담당한다. gRPC의 IDL(Interface Definition Language) 기반이기도 하다. |
| **Abseil (absl)** | `absl` | `absl::flags`, `absl::flags_parse`, `absl::log`, `absl::check` | [abseil/abseil-cpp](https://github.com/abseil/abseil-cpp) | Google 오픈소스 C++ 기본 라이브러리. 커맨드라인 플래그 파싱(`flags`, `flags_parse`), 로깅(`log`), 런타임 검증(`check`)에 사용된다. |
| **Threads** | `Threads` | `Threads::Threads` | CMake 내장 모듈 (`FindThreads`) | POSIX 스레드(pthread) 등 플랫폼별 스레딩 라이브러리를 자동 감지하여 링크한다. 비동기/콜백 서버에서 필수. |

### 외부 도구 (빌드 시점)

| 도구 | 변수명 | 용도 |
|------|--------|------|
| `protoc` | `PROTOC` | `.proto` → C++ 소스(`.pb.cc`, `.pb.h`) 코드 생성 컴파일러 |
| `grpc_cpp_plugin` | `GRPC_CPP_PLUGIN` | `protoc`의 플러그인으로, gRPC 서비스 스텁 코드(`.grpc.pb.cc`, `.grpc.pb.h`)를 생성한다 |

### 코드 생성 흐름

```
helloworld.proto
    │
    ├─ protoc --cpp_out ──────────► helloworld.pb.cc / .pb.h      (메시지 직렬화)
    │
    └─ protoc --grpc_out ─────────► helloworld.grpc.pb.cc / .pb.h (서비스 스텁)
          (via grpc_cpp_plugin)
```

생성된 소스는 `hw_grpc_proto`라는 정적 라이브러리로 묶여, 모든 실행 파일이 공통으로 링크한다.

### 빌드 타겟

`foreach` 루프로 아래 7개 실행 파일을 동일한 방식으로 생성한다. 각 타겟은 `{타겟명}.cpp` 소스 파일 하나와 `hw_grpc_proto` + absl + Threads를 링크한다.

| 타겟 | 패턴 | 설명 |
|------|------|------|
| `greeter_client` | 동기 | 단순 Unary RPC 클라이언트 |
| `greeter_server` | 동기 | 단순 Unary RPC 서버 |
| `greeter_callback_client` | 콜백 | 콜백 기반 비동기 클라이언트 |
| `greeter_callback_server` | 콜백 | 콜백 기반 비동기 서버 |
| `greeter_async_client` | 비동기 | CompletionQueue 기반 비동기 클라이언트 |
| `greeter_async_client2` | 비동기 | CompletionQueue 기반 비동기 클라이언트 (변형) |
| `greeter_async_server` | 비동기 | CompletionQueue 기반 비동기 서버 |

## gRPC RPC 패턴

### CompletionQueue와 Windows IOCP

gRPC의 CompletionQueue는 Windows IOCP와 **동일한 완료 통지(completion notification) 패턴**을 따른다.

| 관점 | CompletionQueue | Windows IOCP |
|------|----------------|--------------|
| 패턴 | 비동기 작업 발행 → 완료 시 큐에서 꺼내 처리 | 동일 |
| 사용 흐름 | `AsyncOp()` → `Next()`로 완료 이벤트 수확 | `PostQueuedCompletionStatus` / `GetQueuedCompletionStatus` |
| 태그/키 | `void*` 태그로 어떤 작업이 완료됐는지 식별 | completion key + OVERLAPPED 포인터로 식별 |
| 계층 | **애플리케이션 레벨** 추상화 (gRPC 라이브러리) | **OS 커널** 수준 I/O 메커니즘 |
| 내부 구현 | 플랫폼별 — Linux: epoll, macOS: kqueue, Windows: IOCP | Windows 전용 커널 오브젝트 |

CompletionQueue는 IOCP에서 영감을 받은 플랫폼별 I/O 멀티플렉서의 **상위 추상화 레이어**다.

### 4가지 RPC 패턴

동기/비동기 여부와 무관하게, **proto 정의**에 의해 통신 패턴이 결정된다.

| 패턴 | Request | Response | proto 정의 |
|------|---------|----------|-----------|
| **Unary** | 1 | 1 | `rpc Method(Request) returns (Reply)` |
| **Server streaming** | 1 | N | `rpc Method(Request) returns (stream Reply)` |
| **Client streaming** | N | 1 | `rpc Method(stream Request) returns (Reply)` |
| **Bidi streaming** | N | N | `rpc Method(stream Request) returns (stream Reply)` |

동기/비동기/콜백은 **구현 방식**의 선택이며, 위 4가지 패턴과 직교한다.

```
             │  Unary (1:1)  │  Server Streaming (1:N)  │  Bidi (N:N)
─────────────┼───────────────┼──────────────────────────┼──────────────
  동기        │      ✓        │          ✓               │      ✓
  비동기(CQ)  │      ✓        │          ✓               │      ✓
  콜백        │      ✓        │          ✓               │      ✓
```

### 비동기 클라이언트의 AsyncClientCall 구조체

`greeter_async_client2.cpp`의 `AsyncClientCall`은 gRPC가 자동 생성하는 코드가 **아니다**. 사용자가 직접 설계해야 한다.

| 구분 | 출처 | 예시 |
|------|------|------|
| **자동 생성** (protoc) | `.proto` → protoc/grpc_cpp_plugin | `HelloRequest`, `HelloReply`, `Greeter::Stub` 등 |
| **사용자 작성** | 개발자가 직접 설계 | `AsyncClientCall`, `GreeterClient`, `say_hello()` 등 |

비동기 패턴에서는 RPC 요청과 완료 처리가 **다른 시점, 다른 스레드**에서 일어나므로, RPC 하나에 필요한 상태(reply, context, status, response_reader)를 묶어 들고 다닐 구조체가 필요하다. CompletionQueue에 `void*` 태그로 등록하고, 완료 시 캐스팅하여 사용한 뒤 `delete`한다.

```
say_hello()                          async_complete_rpc()
   │                                        │
   │  new AsyncClientCall {                  │  큐에서 void* 꺼냄
   │    reply, context, status,              │  → AsyncClientCall*로 캐스팅
   │    response_reader                      │  → reply, status 사용
   │  }                                      │  → delete
   │        ──── CompletionQueue ────►       │
```

### Server Streaming 적용 사례

| 사례 | Request | Response (stream) |
|------|---------|-------------------|
| 실시간 시세 | 종목 코드 | 가격 업데이트 N회 |
| 대용량 검색 | 검색 쿼리 | 결과를 청크 단위로 전송 |
| 로그 tail | 필터 조건 | 로그 라인을 실시간 전달 |
| 파일 다운로드 | 파일 경로 | 바이트 청크 N개 |

### Client Streaming 적용 사례

| 사례 | Request (stream) | Response |
|------|-----------------|----------|
| 파일 업로드 | 바이트 청크 N개 | 저장 결과 (크기, 체크섬) |
| 센서 데이터 수집 | 측정값을 계속 전송 | 집계 결과 (평균, 최대 등) |
| 로그 배치 전송 | 로그 라인 N개 | 수신 확인 (총 건수) |
| 음성 인식 (단방향) | 오디오 청크 N개 | 최종 텍스트 변환 결과 |

### Bidirectional Streaming 적용 사례

| 사례 | Request (stream) | Response (stream) |
|------|-----------------|-------------------|
| 채팅 | 사용자 메시지 N개 | 서버/상대방 메시지 N개 |
| 실시간 번역 | 음성/텍스트 청크 | 번역 결과 청크 |
| 게임 멀티플레이어 | 플레이어 입력 | 게임 상태 업데이트 |
| 협업 편집 | 로컬 편집 이벤트 | 원격 편집 이벤트 |

Server/Client Streaming과의 핵심 차이: **양방향이 독립적으로 동시에 동작**한다. 클라이언트가 전송을 끝내지 않아도 서버가 응답을 보낼 수 있고, 그 반대도 가능하다. `ServerReaderWriter`(서버측) / `ClientReaderWriter`(클라이언트측) 인터페이스를 사용하며, 읽기와 쓰기를 별도 스레드에서 수행하는 것이 일반적이다.

```
Client (thread 1: write)              Server
  │── ChatMessage("Hello") ─────────►│
  │── ChatMessage("How are you?") ──►│  stream->Read() + Write()
  │                                    │
Client (thread 2: read)                │
  │◄── ChatMessage("[echo] Hello") ───│
  │◄── ChatMessage("[echo] How...") ──│
  │                                    │
  │── WritesDone() ──────────────────►│  Read() returns false
  │◄── Stream 종료 ──────────────────│
```

## 관련 예제 프로젝트

| 디렉토리 | 패턴 | 설명 |
|----------|------|------|
| `../server_streaming/` | Server Streaming | 주식 시세 구독 예제 (1 request → N response) |
| `../client_streaming/` | Client Streaming | 센서 데이터 수집 예제 (N request → 1 response) |
| `../bidi_streaming/` | Bidi Streaming | 채팅 에코 예제 (N request ↔ N response) |
