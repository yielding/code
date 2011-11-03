class udp_relay_server
{
private:
  struct relay_policy
  {
    udp::endpoint from_, to_;
    DWORD delay_;
  };

  struct relay_message
  {
    boost::shared_ptr<std::string> message_;
    udp::endpoint destination_;
    DWORD expire_clock_;
  };

  enum
  {
    MIN_SLEEP_DELAY = 10
  };

public:
  udp_relay_server(boost::asio::io_service& io_service, short port)
    : socket_(io_service, udp::endpoint(udp::v4(), port))
    , timer_(io_service)
  {
    do_receive();
    do_timer();
  }

  udp_relay_server(boost::asio::io_service& io_service, const udp::endpoint & endpoint_)
    : socket_(io_service, endpoint_)
    , timer_(io_service)
  {
    do_receive();
    do_timer();
  }

  void add_policy(const udp::endpoint & from_, const udp::endpoint & to_, DWORD delay = 0)
  {
    relay_policy p;
    p.from_ = from_;
    p.to_ = to_;
    p.delay_ = delay;
    policies.push_back(p);
  }

  void add_policy(string from_addr, short from_port, string to_addr, short to_port, DWORD delay = 0)
  {
    udp::endpoint from_(boost::asio::ip::address::from_string(from_addr), from_port);
    udp::endpoint to_(boost::asio::ip::address::from_string(to_addr), to_port);
    add_policy(from_, to_, delay);
  }

  void clear_policy() { policies.clear(); }

private:

  void do_timer()
  {
    timer_.expires_from_now(boost::posix_time::milliseconds(MIN_SLEEP_DELAY));
    timer_.async_wait(boost::bind(&udp_relay_server::handle_timer,this,
                                  boost::asio::placeholders::error));
  }

  void handle_timer(const boost::system::error_code & ec)
  {
    if (!queue_.empty())
    {
      DWORD cur_clock = GetTickCount();
      std::list< relay_message >::iterator itr = queue_.begin();
      while (itr != queue_.end())
      {
        relay_message & m = *itr;
        if (cur_clock > m.expire_clock_)
        {
          socket_.async_send_to(boost::asio::buffer(*m.message_), 
            m.destination_,
            boost::bind(&udp_relay_server::handle_send, this, 
              m.message_, 
              boost::asio::placeholders::error, 
              boost::asio::placeholders::bytes_transferred));
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
      boost::asio::buffer(recv_buffer_), remote_endpoint_,
      boost::bind(&udp_relay_server::handle_receive, this,
        boost::asio::placeholders::error,
        boost::asio::placeholders::bytes_transferred));
  }

  void handle_receive(const boost::system::error_code& error, std::size_t bytes_transferred)
  {
    if (error)
      return;

    bool bRelayed = false;

    for (size_t i=0; i<policies.size(); i++)
    {
      relay_policy & p = policies[i];
      if (p.from_ == remote_endpoint_)
      {
        udp::endpoint relay_to(p.to_);
        boost::shared_ptr<std::string> message(new string(recv_buffer_.data(), bytes_transferred));

        if (p.delay_ > 0)
        {
          relay_message m;
          m.message_ = message;
          m.destination_ = relay_to;
          m.expire_clock_ = GetTickCount() + p.delay_;
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

  void do_send(boost::shared_ptr<std::string> message, const udp::endpoint & destination)
  {
    socket_.async_send_to(boost::asio::buffer(*message), destination,
      boost::bind(&udp_relay_server::handle_send, this,
        message, 
        boost::asio::placeholders::error, 
        boost::asio::placeholders::bytes_transferred));
  }

  void handle_send(boost::shared_ptr<std::string> message, 
      const boost::system::error_code& error, std::size_t bytes_transferred)
  {
  }

  udp::socket socket_;
  udp::endpoint remote_endpoint_;
  boost::array<char, 128> recv_buffer_;
  vector<relay_policy> policies;
  boost::asio::deadline_timer timer_;
  std::list<relay_message> queue_;
};
