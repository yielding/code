#pragma once

#include <system_error>

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
namespace net
{
  using namespace std;

  enum class NetErr { Ok = 0, Timeout, ConnRefused, DnsFail, Proto };

  struct net_error_category : error_category 
  {
    auto name() const noexcept -> const char* override 
    { 
      return "net"; 
    }

    auto message(const int ev) const -> string override 
    {
      switch (static_cast<NetErr>(ev)) 
      {
        case NetErr::Ok:          return "ok";
        case NetErr::Timeout:     return "timeout";
        case NetErr::ConnRefused: return "connection refused";
        case NetErr::DnsFail:     return "DNS failure";
        case NetErr::Proto:       return "protocol error";
      }

      return "unknown net error";
    }
  };

  auto net_category() -> const net_error_category& 
  { 
    static net_error_category c; return c; 
  }

  auto make_error_code(const NetErr e) -> error_code 
  { 
    return { static_cast<int>(e), net_category() }; 
  }
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
