#include "nats_util.hpp"

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
namespace natsx
{
  using namespace std;

  namespace
  {
    auto describe(const natsStatus status, const string context) -> string
    {
      auto res = context + ": " + natsStatus_GetText(status);
      if (auto detail = nats_GetLastError(nullptr); detail != nullptr && *detail != '\0')
        res += string(" (") + detail + ")";

      return res;
    }
  }

  ////////////////////////////////////////////////////////////////////////////////
  //
  // nats_error
  //
  ////////////////////////////////////////////////////////////////////////////////
  nats_error::nats_error(const natsStatus status, const string message)
    : runtime_error(message)
    , _status(status)
  {
  }

  auto nats_error::status() const -> natsStatus
  {
    return _status;
  }

  auto check(const natsStatus status, const string context) -> void
  {
    if (status == NATS_OK)
      return;

    throw nats_error(status, describe(status, context));
  }

  ////////////////////////////////////////////////////////////////////////////////
  //
  // deleters / accessors
  //
  ////////////////////////////////////////////////////////////////////////////////
  auto msg_deleter::operator()(natsMsg* p) const -> void
  {
    natsMsg_Destroy(p);
  }

  auto sub_deleter::operator()(natsSubscription* p) const -> void
  {
    natsSubscription_Destroy(p);
  }

  auto conn_deleter::operator()(natsConnection* p) const -> void
  {
    natsConnection_Destroy(p);
  }

  auto js_deleter::operator()(jsCtx* p) const -> void
  {
    jsCtx_Destroy(p);
  }

  auto data_of(const natsMsg* msg) -> string_view
  {
    if (msg == nullptr)
      return {};

    auto data = natsMsg_GetData(msg);
    auto size = natsMsg_GetDataLength(msg);

    return data == nullptr ? string_view{} : string_view(data, size_t(size));
  }

  auto subject_of(const natsMsg* msg) -> string_view
  {
    auto subject = msg == nullptr ? nullptr : natsMsg_GetSubject(msg);

    return subject == nullptr ? string_view{} : string_view(subject);
  }

  auto reply_of(const natsMsg* msg) -> string_view
  {
    auto reply = msg == nullptr ? nullptr : natsMsg_GetReply(msg);

    return reply == nullptr ? string_view{} : string_view(reply);
  }

  ////////////////////////////////////////////////////////////////////////////////
  //
  // subscription
  //
  ////////////////////////////////////////////////////////////////////////////////
  subscription::subscription(natsSubscription* sub)
    : _sub(sub)
  {
  }

  auto subscription::next(const int64_t timeout_ms) -> msg_ptr
  {
    natsMsg* msg{};

    auto s = natsSubscription_NextMsg(&msg, _sub.get(), timeout_ms);
    if (s == NATS_TIMEOUT)
      return {};

    check(s, "natsSubscription_NextMsg");

    return msg_ptr{msg};
  }

  auto subscription::fetch(const int batch, const int64_t timeout_ms) -> vector<msg_ptr>
  {
    natsMsgList list{};
    jsErrCode   jerr{};

    auto s = natsSubscription_Fetch(&list, _sub.get(), batch, timeout_ms, &jerr);
    if (s == NATS_TIMEOUT)
      return {};

    check(s, "natsSubscription_Fetch");

    vector<msg_ptr> res;
    res.reserve(size_t(list.Count));
    for (auto i = 0; i < list.Count; ++i)
    {
      res.emplace_back(list.Msgs[i]);
      list.Msgs[i] = nullptr;         // 소유권을 msg_ptr로 넘긴다
    }
    natsMsgList_Destroy(&list);

    return res;
  }

  auto subscription::drain() const -> void
  {
    if (_sub)
      natsSubscription_Drain(_sub.get());
  }

  auto subscription::handle() const -> natsSubscription*
  {
    return _sub.get();
  }

  ////////////////////////////////////////////////////////////////////////////////
  //
  // js_context
  //
  ////////////////////////////////////////////////////////////////////////////////
  js_context::js_context(jsCtx* js)
    : _js(js)
  {
  }

  auto js_context::ensure_stream(const string name, const vector<string> subjects) -> void
  {
    vector<const char*> filters;
    filters.reserve(subjects.size());
    for (auto const& s: subjects)
      filters.push_back(s.c_str());

    jsStreamConfig cfg;
    jsStreamConfig_Init(&cfg);
    cfg.Name        = name.c_str();
    cfg.Subjects    = filters.data();
    cfg.SubjectsLen = int(filters.size());
    cfg.Storage     = js_FileStorage;

    jsStreamInfo* info{};
    jsErrCode     jerr{};

    auto s = js_AddStream(&info, _js.get(), &cfg, nullptr, &jerr);
    if (s != NATS_OK && jerr == JSStreamNameExistErr)
      s = js_UpdateStream(&info, _js.get(), &cfg, nullptr, &jerr);

    check(s, "js_AddStream(" + name + ")");
    jsStreamInfo_Destroy(info);
  }

  auto js_context::publish(const string subject, const string payload) -> uint64_t
  {
    jsPubAck* ack{};
    jsErrCode jerr{};

    auto s = js_Publish(&ack, _js.get()
                      , subject.c_str(), payload.data(), int(payload.size())
                      , nullptr, &jerr);
    check(s, "js_Publish(" + subject + ")");

    auto seq = ack->Sequence;
    jsPubAck_Destroy(ack);

    return seq;
  }

  auto js_context::pull_subscribe(const string subject
                                , const string durable
                                , const string stream) -> subscription
  {
    jsSubOptions opts;
    jsSubOptions_Init(&opts);
    opts.Config.AckPolicy = js_AckExplicit;
    if (!stream.empty())
      opts.Stream = stream.c_str();

    natsSubscription* sub{};
    jsErrCode         jerr{};

    auto s = js_PullSubscribe(&sub, _js.get()
                            , subject.c_str(), durable.c_str()
                            , nullptr, &opts, &jerr);
    check(s, "js_PullSubscribe(" + subject + ")");

    return subscription{sub};
  }

  auto js_context::handle() const -> jsCtx*
  {
    return _js.get();
  }

  ////////////////////////////////////////////////////////////////////////////////
  //
  // connection
  //
  ////////////////////////////////////////////////////////////////////////////////
  connection::connection(const string url)
  {
    natsConnection* conn{};

    check(natsConnection_ConnectTo(&conn, url.c_str()), "natsConnection_ConnectTo(" + url + ")");
    _conn.reset(conn);
  }

  auto connection::publish(const string subject, const string payload) -> void
  {
    auto s = natsConnection_Publish(_conn.get()
                                  , subject.c_str(), payload.data(), int(payload.size()));
    check(s, "natsConnection_Publish(" + subject + ")");
  }

  auto connection::request(const string subject
                         , const string payload
                         , const int64_t timeout_ms) -> msg_ptr
  {
    natsMsg* reply{};

    auto s = natsConnection_Request(&reply, _conn.get()
                                  , subject.c_str(), payload.data(), int(payload.size())
                                  , timeout_ms);
    if (s == NATS_TIMEOUT || s == NATS_NO_RESPONDERS)
      return {};

    check(s, "natsConnection_Request(" + subject + ")");

    return msg_ptr{reply};
  }

  auto connection::subscribe(const string subject
                           , natsMsgHandler handler
                           , void* closure) -> subscription
  {
    natsSubscription* sub{};

    auto s = natsConnection_Subscribe(&sub, _conn.get(), subject.c_str(), handler, closure);
    check(s, "natsConnection_Subscribe(" + subject + ")");

    return subscription{sub};
  }

  auto connection::subscribe_sync(const string subject) -> subscription
  {
    natsSubscription* sub{};

    auto s = natsConnection_SubscribeSync(&sub, _conn.get(), subject.c_str());
    check(s, "natsConnection_SubscribeSync(" + subject + ")");

    return subscription{sub};
  }

  auto connection::respond(const natsMsg* request, const string payload) -> void
  {
    auto reply = reply_of(request);
    if (reply.empty())
      return;

    auto s = natsConnection_Publish(_conn.get()
                                  , string(reply).c_str(), payload.data(), int(payload.size()));
    check(s, "natsConnection_Publish(reply)");
  }

  auto connection::flush() -> void
  {
    check(natsConnection_Flush(_conn.get()), "natsConnection_Flush");
  }

  auto connection::jet_stream() -> js_context
  {
    jsOptions opts;
    jsOptions_Init(&opts);
    opts.Wait = 5000;

    jsCtx* js{};
    check(natsConnection_JetStream(&js, _conn.get(), &opts), "natsConnection_JetStream");

    return js_context{js};
  }

  auto connection::handle() const -> natsConnection*
  {
    return _conn.get();
  }

  ////////////////////////////////////////////////////////////////////////////////
  //
  // library_guard
  //
  ////////////////////////////////////////////////////////////////////////////////
  library_guard::~library_guard()
  {
    nats_Close();
  }
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
