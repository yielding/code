#include <boost/asio/connect.hpp>
#include <boost/asio/deadline_timer.hpp>
#include <boost/asio/io_service.hpp>
#include <boost/asio/ip/tcp.hpp>
#include <boost/asio/read_until.hpp>
#include <boost/asio/streambuf.hpp>
#include <boost/asio/write.hpp>
#include <boost/system/system_error.hpp>
#include <boost/lambda/bind.hpp>
#include <boost/lambda/lambda.hpp>

#include <iostream>
#include <string>

using boost::asio::deadline_timer;
using boost::asio::ip::tcp;
using boost::lambda::bind;
using boost::lambda::var;
using boost::lambda::_1;

using namespace std;
using namespace boost;

class client 
{
public:
    client() : _socket(_ios), _deadline(_ios)
    {
        _deadline.expires_at(posix_time::pos_infin); 
        check_deadline();
    }

    ~client()
    {
    }

    void connect(string const& host, string const& service, posix_time::time_duration timeout)
    {
        tcp::resolver::query query(tcp::v4(), host, service);
        auto iter = tcp::resolver(_ios).resolve(query);
        _deadline.expires_from_now(timeout);

        system::error_code ec = asio::error::would_block;
        asio::async_connect(_socket, iter, var(ec) = _1);

        do _ios.run_one(); while (ec == asio::error::would_block);
        if (ec || !_socket.is_open())
            throw system::system_error(ec ? ec : asio::error::operation_aborted);
    }

    string read_all(posix_time::time_duration timeout)
    {
        _deadline.expires_from_now(timeout);
        //asio::read_until(_socket, _input_buffer, "\r\n\r\n");
        system::error_code ec = asio::error::would_block;
        asio::async_read_until(_socket, _input_buffer, "\r\n\r\n", var(ec) = _1);
        do _ios.run_one(); while (ec == asio::error::would_block);
        if (ec)
            throw system::system_error(ec);

        auto sz = _input_buffer.size();
        if (sz <= 0) return "";

        std::istream is(&_input_buffer);
        char res[1024*100];
        is.read(res, sz);

        return string(res);
    }

    void write_line(string const& line, posix_time::time_duration timeout)
    {
        string data = line + "\n\r";

        _deadline.expires_from_now(timeout);

        system::error_code ec = asio::error::would_block;

        asio::async_write(_socket, asio::buffer(data), var(ec) = _1);

        // Block until the asynchronous operation has completed.
        do _ios.run_one(); while (ec == asio::error::would_block);

        if (ec)
            throw system::system_error(ec);
    }

private:
    void check_deadline()
    {
        if (_deadline.expires_at() <= deadline_timer::traits_type::now())
        {
            system::error_code ignored_ec;
            _socket.close(ignored_ec);

            _deadline.expires_at(posix_time::pos_infin);
        }

        _deadline.async_wait(bind(&client::check_deadline, this));
    }

private:
    asio::io_service _ios;
    tcp::socket _socket;
    asio::deadline_timer _deadline;
    asio::streambuf _input_buffer;
};


int main(int argc, const char *argv[])
{
    client c;
    c.connect("218.145.28.200", "http", posix_time::seconds(10));
    c.write_line("GET", posix_time::seconds(10));
    auto line = c.read_all(posix_time::seconds(10));
    cout << line;

    return 0;
}
