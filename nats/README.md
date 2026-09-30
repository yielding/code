# NATS core + JetStream C++ 예제

[nats.c](https://github.com/nats-io/nats.c)(cnats) 위에 얇은 RAII 래퍼를 씌워
**core NATS(request-reply / pub-sub)** 와 **JetStream(영속 스트림)** 을 각각
server / client 한 쌍으로 구현한 예제.

> NATS 에서 "서버"는 브로커인 `nats-server` 를 뜻하고 응용 프로그램은 전부 클라이언트다.
> 여기서 `*_server` 는 **요청을 받아 처리하는 쪽(응답자/소비자)**,
> `*_client` 는 **요청을 보내는 쪽(요청자/발행자)** 이라는 의미로 쓴다.

## 구성

| 파일 | 역할 |
|------|------|
| `nats_util.hpp` / `nats_util.cpp` | cnats C API 를 감싼 RAII 래퍼 (`connection`, `subscription`, `js_context`, 예외 변환) |
| `core_server.cpp` | core NATS 응답자. `demo.greet` 에 request-reply, `demo.event.>` 는 로그만 |
| `core_client.cpp` | core NATS 요청자. `demo.greet` 로 request, `demo.event.tick` 으로 fire-and-forget |
| `js_client.cpp` | JetStream 발행자. `DEMO` 스트림 보장 후 ack 를 기다리는 동기 발행 |
| `js_server.cpp` | JetStream 소비자. durable pull 컨슈머로 배치 fetch 후 명시적 ack |
| `nats-server.conf` | JetStream 을 켠 브로커 설정 |
| `CMakeLists.txt` | 빌드. cnats 가 설치돼 있으면 그것을, 없으면 소스를 받아 static 으로 함께 빌드 |

## 준비물

### nats-server

```bash
# 데비안/우분투
sudo apt install nats-server

# 또는 릴리스 바이너리
curl -sfL https://github.com/nats-io/nats-server/releases/download/v2.14.4/nats-server-v2.14.4-linux-amd64.tar.gz \
  | tar xz --strip-components=1 -C /usr/local/bin nats-server-v2.14.4-linux-amd64/nats-server
```

### 클라이언트 라이브러리 (cnats)

별도 설치는 **선택**이다. 설치돼 있지 않으면 CMake 가 `FetchContent` 로
nats.c `v3.9.3` 을 받아 static 으로 같이 빌드한다. 미리 깔아두려면:

```bash
sudo apt install libnats-dev          # find_package(cnats CONFIG) 로 자동 인식
```

TLS 지원 때문에 OpenSSL 개발 헤더가 필요하다 (`sudo apt install libssl-dev`).

## 빌드

```bash
cmake -S . -B build -DCMAKE_BUILD_TYPE=Release
cmake --build build -j
```

산출물: `build/core_server`, `build/core_client`, `build/js_server`, `build/js_client`.
모두 첫 번째 인자로 접속 URL 을 받으며 기본값은 `nats://localhost:4222` 다.

## 실행

### 0. 브로커 기동

```bash
nats-server -c nats-server.conf          # JetStream 포함
# 또는
nats-server -js -sd ./jsdata
```

### 1. core NATS — request-reply + pub-sub

```bash
# 터미널 A
./build/core_server

# 터미널 B
./build/core_client
```

```
[core.client] reply    -> hello, client-1!
[core.server] request  <- demo.greet : client-1
[core.server] event    <- demo.event.tick : tick 1
```

`core_server` 를 띄우지 않고 `core_client` 만 실행하면 `no responder / timeout` 이 찍힌다.
core NATS 는 **at-most-once**, 즉 받는 사람이 없으면 메시지는 그냥 사라진다.

### 2. JetStream — 영속 스트림

```bash
# 발행부터 먼저 해도 된다. 소비자가 없어도 서버에 저장된다
./build/js_client

# 나중에 소비자를 띄우면 저장된 것부터 순서대로 받는다
./build/js_server
```

```
[js.client] published -> demo.js.order : order #1 (seq 1)
[js.server] consumed <- demo.js.order : order #1
```

확인해볼 만한 동작:

- **재개**: `js_server` 를 Ctrl-C 로 끄고 `js_client` 를 더 실행한 뒤 다시 켜면,
  이미 ack 한 메시지는 건너뛰고 새 것부터 받는다 (durable 이름 `demo-worker` 기준).
- **분산**: `js_server` 를 두 개 띄우면 같은 durable 컨슈머를 공유하므로
  메시지가 둘에 나뉘어 배분된다.
- **상태 조회**: `curl -s localhost:8222/jsz?streams=1 | jq` 또는 `nats stream info DEMO`.

## core 와 JetStream 의 차이

| | core NATS | JetStream |
|---|---|---|
| 전달 보장 | at-most-once (구독자 없으면 소실) | at-least-once (스트림에 저장 후 ack) |
| 발행 API | `natsConnection_Publish` — 즉시 반환 | `js_Publish` — 서버 저장 ack 대기, 시퀀스 반환 |
| 소비 상태 | 없음 (연결이 곧 상태) | 컨슈머가 서버에 남음, durable 이면 재시작 후 이어받기 |
| 서버 설정 | 불필요 | `jetstream {}` 블록 또는 `-js` 필요 |
| 이 예제 | `core_server` / `core_client` | `js_server` / `js_client` |

## 래퍼 설계 메모

- 모든 C 핸들(`natsConnection*`, `natsSubscription*`, `natsMsg*`, `jsCtx*`)은
  `unique_ptr` + 전용 deleter 로 감쌌다. 수동 `*_Destroy` 호출이 없다.
- `natsStatus` 는 `check()` 에서 `nats_error` 예외로 바꾼다.
  `nats_GetLastError()` 의 상세 메시지까지 붙여서 던진다.
- 비동기 콜백(`natsMsgHandler`)은 C 시그니처를 그대로 써야 한다.
  콜백에 들어온 `natsMsg*` 의 소유권은 콜백이 가지므로 진입 즉시 `msg_ptr` 로 감싼다.
- `subscription::fetch()` 는 `natsMsgList` 의 각 원소를 `msg_ptr` 로 옮기고
  원본 슬롯을 `nullptr` 로 만든 뒤 리스트를 파괴해 이중 해제를 막는다.
- `library_guard` 를 `main` 첫 줄에 두면 종료 시 `nats_Close()` 가 불린다.
