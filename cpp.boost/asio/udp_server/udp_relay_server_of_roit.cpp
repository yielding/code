#include <boost/asio.hpp>
#include <boost/noncopyable.hpp>
#include <boost/shared_ptr.hpp>
#include <boost/thread/thread.hpp>
#include <boost/bind.hpp>
#include <boost/array.hpp>

#include <iostream>

using namespace std;
using namespace boost;
using namespace boost::asio;

using boost::asio::ip::udp;
using boost::asio::ip::udp::endpoint;
//////////////////////////////////////////////////////////////////////////////
//
//
//
//////////////////////////////////////////////////////////////////////////////
class udp_relay_server
{
  struct relay_policy
  {
    endpoint m_from, m_to;
    DWORD    m_delay;
  };

  struct relay_message
  {
    shared_ptr<string> m_message;
    endpoint m_destination;
    DWORD m_expire_clock;
  };

  enum { MIN_SLEEP_DELAY = 10 };

public:

  udp_relay_server(io_service& io_service, short port)
    : socket_(io_service, endpoint(udp::v4(), port))
    , timer_(io_service)
  {
    do_receive();
    do_timer();
  }

  udp_relay_server(io_service& io_service, endpoint const& endpoint_)
    : socket_(io_service, endpoint_)
    , timer_(io_service)
  {
    do_receive();
    do_timer();
  }

  void add_policy(endpoint const& m_from, endpoint const& m_to, DWORD delay = 0)
  {
    relay_policy p;
    p.m_from = m_from;
    p.m_to = m_to;
    p.m_delay = delay;
    policies.push_back(p);
  }

  void add_policy(string m_fromaddr, short m_fromport, string m_toaddr, short m_toport, DWORD delay = 0)
  {
    endpoint m_from(ip::address::m_fromstring(m_fromaddr), m_fromport);
    endpoint m_to(ip::address::m_fromstring(m_toaddr), m_toport);
    add_policy(m_from, m_to, delay);
  }

  void clear_policy() 
  { 
    policies.clear(); 
  }

private:

  void do_timer()
  {
    timer_.expires_from_now(posix_time::milliseconds(MIN_SLEEP_DELAY));
    timer_.async_wait(bind(&udp_relay_server::handle_timer,this, placeholders::error));
  }

  void handle_timer(const system::error_code & ec)
  {
    if (!queue_.empty())
    {
      DWORD cur_clock = GetTickCount();
      list<relay_message>::iterator itr = queue_.begin();
      while (itr != queue_.end())
      {
        relay_message & m = *itr;
        if (cur_clock > m.m_expire_clock)
        {
          socket_.async_send_to(buffer(*m.m_message), m.m_destination,
              bind(&udp_relay_server::handle_send, this, m.m_message, placeholders::error, placeholders::bytes_transferred));
          itr = queue_.erase(itr++);
        }
        else
        {
          itr++;
        }
      }
    }
    do_timer();
  }

  void do_receive()
  {
    socket_.async_receive_from(
      buffer(recv_buffer_), remote_endpoint_,
      bind(&udp_relay_server::handle_receive, this,
        placeholders::error,
        placeholders::bytes_transferred));
  }

  void handle_receive(const system::error_code& error, size_t bytes_transferred)
  {
    if (!error)
    {
      bool bRelayed = false;
      for (size_t i=0; i<policies.size(); i ++)
      {
        relay_policy & p = policies[i];
        if (p.m_from == remote_endpoint_)
        {
          endpoint relay_to(p.m_to);
          shared_ptr<string> message(new string(recv_buffer_.data(), bytes_transferred));
          if (p.m_delay > 0)
          {
            relay_message m;
            m.m_message = message;
            m.m_destination = relay_to;
            m.m_expire_clock = GetTickCount() + p.m_delay;
            queue_.push_back(m);
          }
          else
          {
            do_send(message, relay_to);
          }
          bRelayed = true;
          break;
        }
      }
      do_receive();
    }
  }

  void do_send(shared_ptr<string> message, const endpoint & destination)
  {
    socket_.async_send_to(buffer(*message), destination,
        bind(&udp_relay_server::handle_send, this,
          message, placeholders::error, placeholders::bytes_transferred));
  }

  void handle_send(shared_ptr<string> message, 
      const system::error_code& error, size_t bytes_transferred)
  {
  }

  ip::udp::socket socket_;
  endpoint remote_endpoint_;
  array<char, 128> recv_buffer_;
  vector<relay_policy> policies;
  deadline_timer timer_;
  list<relay_message> queue_;
};

//////////////////////////////////////////////////////////////////////////////
//
//
//
//////////////////////////////////////////////////////////////////////////////
