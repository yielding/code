#pragma once

#include "domain_error.hpp"
#include "net_error.hpp"
#include "db_error.hpp"

#include <expected>
#include <string>
#include <string_view>

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
namespace service
{
  using namespace std;
  using namespace domain;
  using namespace net;
  using namespace db;

  inline auto http_get(const string_view host) -> Ex<string> 
  {
    if (host == "timeout")
      return Err(make_error_code(NetErr::Timeout), Source::Net, 
                "slow upstream", string(host), "GET");

    if (host == "bad-proto")
      return Err(make_error_code(NetErr::Proto), Source::Net, "frame error", string(host), "GET");

    return string{"{ \"user\": 42 }"};
  }

  inline auto db_query_user(const string_view sql) -> Ex<int> 
  {
    if (sql == "deadlock")
      return Err(make_error_code(DbErr::Deadlock), Source::Db, {}, string(sql), "SELECT");

    if (sql == "bad")
      return Err(make_error_code(DbErr::Syntax), Source::Db, {}, string(sql), "SELECT");

    return 42;
  }

  inline auto get_user_id(const string_view host) -> Ex<int> 
  {
    auto resp = http_get(host);
    if (!resp) 
      return unexpected(resp.error());

    if (resp->find("user") == string::npos)
      return Err(make_error_code(NetErr::Proto), Source::Net, "missing 'user' field", string(host), "parse");

    auto uid = db_query_user("SELECT id FROM users WHERE name='alice'");
    if (!uid) 
      return unexpected(uid.error());

    return *uid;
  }
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
