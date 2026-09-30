#pragma once

#if __has_include(<nats/nats.h>)
#include <nats/nats.h>
#else
#include <nats.h>
#endif

#include <cstdint>
#include <memory>
#include <stdexcept>
#include <string>
#include <string_view>
#include <vector>

////////////////////////////////////////////////////////////////////////////////
//
// cnats(C API) 위에 올린 얇은 RAII 래퍼
//
////////////////////////////////////////////////////////////////////////////////
namespace natsx
{
  using namespace std;

  ////////////////////////////////////////////////////////////////////////////////
  //
  // 예외
  //
  ////////////////////////////////////////////////////////////////////////////////
  class nats_error: public runtime_error
  {
  public:
    nats_error(const natsStatus status, const string message);

  public:
    auto status() const -> natsStatus;

  private:
    natsStatus _status;
  };

  auto check(const natsStatus status, const string context) -> void;

  ////////////////////////////////////////////////////////////////////////////////
  //
  // C 핸들 소유권
  //
  ////////////////////////////////////////////////////////////////////////////////
  struct msg_deleter  { auto operator()(natsMsg* p)          const -> void; };
  struct sub_deleter  { auto operator()(natsSubscription* p) const -> void; };
  struct conn_deleter { auto operator()(natsConnection* p)   const -> void; };
  struct js_deleter   { auto operator()(jsCtx* p)            const -> void; };

  using msg_ptr  = unique_ptr<natsMsg, msg_deleter>;
  using sub_ptr  = unique_ptr<natsSubscription, sub_deleter>;
  using conn_ptr = unique_ptr<natsConnection, conn_deleter>;
  using js_ptr   = unique_ptr<jsCtx, js_deleter>;

  auto data_of(const natsMsg* msg)    -> string_view;
  auto subject_of(const natsMsg* msg) -> string_view;
  auto reply_of(const natsMsg* msg)   -> string_view;

  ////////////////////////////////////////////////////////////////////////////////
  //
  // 구독 (core / JetStream 공용)
  //
  ////////////////////////////////////////////////////////////////////////////////
  class subscription
  {
  public:
    subscription() = default;
    explicit subscription(natsSubscription* sub);

  public:
    // 동기 구독에서 다음 메시지를 꺼낸다. 타임아웃이면 빈 포인터
    auto next(const int64_t timeout_ms) -> msg_ptr;

    // JetStream pull 구독에서 최대 batch개를 당겨온다. 타임아웃이면 빈 벡터
    auto fetch(const int batch, const int64_t timeout_ms) -> vector<msg_ptr>;

    auto drain()  const -> void;
    auto handle() const -> natsSubscription*;

  private:
    sub_ptr _sub;
  };

  ////////////////////////////////////////////////////////////////////////////////
  //
  // JetStream 컨텍스트
  //
  ////////////////////////////////////////////////////////////////////////////////
  class js_context
  {
  public:
    js_context() = default;
    explicit js_context(jsCtx* js);

  public:
    // 스트림이 없으면 만들고, 있으면 설정을 갱신한다
    auto ensure_stream(const string name, const vector<string> subjects) -> void;

    // 서버 ack를 기다리는 동기 발행. 스트림 시퀀스를 돌려준다
    auto publish(const string subject, const string payload) -> uint64_t;

    // durable pull 컨슈머를 만든다(이미 있으면 그대로 붙는다)
    auto pull_subscribe(const string subject
                      , const string durable
                      , const string stream) -> subscription;

    auto handle() const -> jsCtx*;

  private:
    js_ptr _js;
  };

  ////////////////////////////////////////////////////////////////////////////////
  //
  // 연결
  //
  ////////////////////////////////////////////////////////////////////////////////
  class connection
  {
  public:
    explicit connection(const string url = NATS_DEFAULT_URL);

  public:
    auto publish(const string subject, const string payload) -> void;
    auto request(const string subject
               , const string payload
               , const int64_t timeout_ms) -> msg_ptr;

    auto subscribe(const string subject, natsMsgHandler handler, void* closure) -> subscription;
    auto subscribe_sync(const string subject) -> subscription;

    auto respond(const natsMsg* request, const string payload) -> void;
    auto flush() -> void;

    auto jet_stream() -> js_context;
    auto handle() const -> natsConnection*;

  private:
    conn_ptr _conn;
  };

  ////////////////////////////////////////////////////////////////////////////////
  //
  // main 진입점에 하나 두면 종료 시 라이브러리 전역 자원을 정리한다
  //
  ////////////////////////////////////////////////////////////////////////////////
  class library_guard
  {
  public:
    library_guard() = default;
    ~library_guard();
  };
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
