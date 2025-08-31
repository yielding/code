#include "expected_util.hpp"
#include "domain_error.hpp"
#include "service.hpp"

#include <print>

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
using namespace expx;
using namespace domain;
using namespace service;
using namespace std;

static auto get_net_error(const error_code& ec) -> net::NetErr 
{
  return static_cast<net::NetErr>(ec.value());
}

static auto get_db_error(const error_code& ec) -> db::DbErr 
{
  return static_cast<db::DbErr>(ec.value());
}

static auto print_user_error(const DomainError& de) -> void 
{
  if (de.src == Source::Net) 
  {
    switch (get_net_error(de.code)) 
    {
      case net::NetErr::Timeout:
        println(stderr, "[user] 네트워크 타임아웃입니다."); return;
      case net::NetErr::ConnRefused:
        println(stderr, "[user] 서버 연결이 거부되었습니다."); return;
      case net::NetErr::DnsFail:
        println(stderr, "[user] DNS 오류입니다."); return;
      default:
        println(stderr, "[user] 네트워크 오류가 발생했습니다."); return;
    }
  } 
  else if (de.src == Source::Db) 
  {
    switch (get_db_error(de.code)) 
    {
      case db::DbErr::Deadlock:
        println(stderr, "[user] 잠시 후 다시 시도해 주세요."); return;
      case db::DbErr::Constraint:
        println(stderr, "[user] 데이터 제약 조건 오류입니다."); return;
      default:
        println(stderr, "[user] 데이터베이스 오류가 발생했습니다."); return;
    }
  } 
  else 
  {
    println(stderr, "[user] 처리 중 오류가 발생했습니다.");
  }
}

int main() 
{
  // 성공 케이스
  if (auto r = get_user_id("api"); r) 
  {
    println("OK user_id={}", *r);
  } 
  else 
  {
    println(stderr, "[internal] {}", r.error().message());
    print_user_error(r.error());
  }

  // 네트워크 타임아웃
  if (auto r = get_user_id("timeout"); !r) 
  {
    println(stderr, "[internal] {}", r.error().message());
    print_user_error(r.error());
  }

  // 네트워크 프로토콜 오류(파싱 실패)
  if (auto r = get_user_id("bad-proto"); !r) 
  {
    println(stderr, "[internal] {}", r.error().message());
    print_user_error(r.error());
  }

  // DB 데드락
  if (auto r = db_query_user("deadlock"); !r) 
  {
    println(stderr, "[internal] {}", r.error().message());
    print_user_error(r.error());
  }
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
