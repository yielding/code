#include <boost/asio.hpp>
#include <boost/asio/ssl.hpp>
#include <iostream>

namespace asio = boost::asio;
namespace sys  = boost::system;

using tcp = asio::ip::tcp;
using namespace std;

int main() 
{
  try 
  {
    asio::io_context io;
    asio::ssl::context tls_ctx(asio::ssl::context::tls_server);

    // 보안 옵션: TLS1.2 이상, 압축 비활성
    ::SSL_CTX_set_min_proto_version(tls_ctx.native_handle(), TLS1_2_VERSION);
    ::SSL_CTX_set_options(tls_ctx.native_handle(),
      SSL_OP_NO_SSLv2 | SSL_OP_NO_SSLv3 | SSL_OP_NO_TLSv1 | SSL_OP_NO_TLSv1_1 | SSL_OP_NO_COMPRESSION);

    // 서버 인증서/키
    tls_ctx.use_certificate_chain_file("server.crt");
    tls_ctx.use_private_key_file("server.key", asio::ssl::context::pem);

    // (mTLS) 클라이언트 인증 강제하려면 주석 해제
    // tls_ctx.load_verify_file("client.crt"); // 또는 CA 번들
    // tls_ctx.set_verify_mode(asio::ssl::verify_peer | asio::ssl::verify_fail_if_no_peer_cert);

    tcp::acceptor acc(io, {tcp::v4(), 8443});
    for (;;) 
    {
      tcp::socket raw(io);
      acc.accept(raw);

      auto ssl = make_shared<asio::ssl::stream<tcp::socket>>(std::move(raw), tls_ctx);

      // 핸드셰이크 타임아웃 (예: 5초)
      auto timer = make_shared<asio::steady_timer>(io, chrono::seconds(5));
      bool timed_out = false;
      timer->async_wait([ssl, &timed_out](const sys::error_code& ec) {
        if (!ec) { timed_out = true; sys::error_code ignore; ssl->lowest_layer().close(ignore); }
      });

      sys::error_code ec;
      ssl->handshake(asio::ssl::stream_base::server, ec);
      timer->cancel();

      if (timed_out || ec) 
      {
        cerr << "handshake failed: " << (timed_out ? "timeout" : ec.message()) << "\n";
        continue;
      }

      // 에코 1회 (데모)
      array<char, 1024> buf{};
      size_t n = ssl->read_some(asio::buffer(buf), ec);
      if (!ec) asio::write(*ssl, asio::buffer(buf.data(), n));
    }
  } 
  catch (exception& e) 
  {
    cerr << "server exception: " << e.what() << "\n";
    return 1;
  }
}