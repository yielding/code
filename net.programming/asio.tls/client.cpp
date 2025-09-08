#include <boost/asio.hpp>
#include <boost/asio/ssl.hpp>
#include <iostream>

namespace asio = boost::asio;
namespace sys  = boost::system;

using tcp = asio::ip::tcp;
using namespace std;

int main(int argc, char** argv) 
{
  const string host = (argc > 1) ? argv[1] : "localhost";
  const string port = (argc > 2) ? argv[2] : "8443";

  try 
  {
    asio::io_context io;
    asio::ssl::context tls_ctx(asio::ssl::context::tls_client);

    // TLS 1.2+, 압축 금지
    ::SSL_CTX_set_min_proto_version(tls_ctx.native_handle(), TLS1_2_VERSION);
    ::SSL_CTX_set_options(tls_ctx.native_handle(),
      SSL_OP_NO_SSLv2 | SSL_OP_NO_SSLv3 | SSL_OP_NO_TLSv1 | SSL_OP_NO_TLSv1_1 | SSL_OP_NO_COMPRESSION);

    // 신뢰 루트: 테스트에서는 서버 CRT를 직접 신뢰(데모)
    tls_ctx.load_verify_file("server.crt");
    tls_ctx.set_verify_mode(asio::ssl::verify_peer);

    // (mTLS) 필요 시 클라이언트 인증서/키
    // tls_ctx.use_certificate_file("client.crt", asio::ssl::context::pem);
    // tls_ctx.use_private_key_file("client.key", asio::ssl::context::pem);

    asio::ssl::stream<tcp::socket> ssl(io, tls_ctx);

    // SNI 설정
    ::SSL_set_tlsext_host_name(ssl.native_handle(), host.c_str());

    // 호스트명 검증(RFC6125)
    ssl.set_verify_callback([&](bool preverified, asio::ssl::verify_context& ctx) {
      X509* cert = X509_STORE_CTX_get_current_cert(ctx.native_handle());
      if (!preverified || !cert) return false;
      return ::X509_check_host(cert, host.c_str(), host.size(), 0, nullptr) == 1;
    });

    tcp::resolver r(io);
    auto eps = r.resolve(host, port);
    asio::connect(ssl.next_layer(), eps);

    // 핸드셰이크 타임아웃 (5초)
    asio::steady_timer timer(io, chrono::seconds(5));
    bool timed_out = false;
    timer.async_wait([&](const sys::error_code& ec) {
      if (!ec) { timed_out = true; sys::error_code ignore; ssl.lowest_layer().close(ignore); }
    });

    sys::error_code ec;
    ssl.handshake(asio::ssl::stream_base::client, ec);
    timer.cancel();
    if (timed_out || ec) {
      std::cerr << "handshake failed: " << (timed_out ? "timeout" : ec.message()) << "\n";
      return 1;
    }

    // 에코 1회 (데모)
    auto msg = "hello over TLS\n"s;
    asio::write(ssl, asio::buffer(msg));
    array<char, 1024> buf{};
    size_t n = ssl.read_some(asio::buffer(buf), ec);
    if (!ec) cout << string(buf.data(), n);

  } 
  catch (exception& e) 
  {
    cerr << "client exception: " << e.what() << "\n";
    return 1;
  }
}
