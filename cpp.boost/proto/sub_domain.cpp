#include <boost/proto/proto.hpp>
namespace proto = boost::proto;

// Forward-declare two expression wrappers
template<typename E> struct spirit_expr;
template<typename E> struct phoenix_expr;

// Define two domains
struct spirit_domain
  : proto::domain<proto::generator<spirit_expr> > {};

// struct phoenix_domain : proto::domain<proto::generator<phoenix_expr> > {};

//struct phoenix_domain
//  : proto::domain<proto::generator<phoenix_expr>, proto::_, spirit_domain>
//{};

struct phoenix_domain
  : proto::domain<proto::generator<phoenix_expr>, proto::_, proto::default_domain>
{}; // 다른 도메인과 자유롭게 섞일 수 있게 디자인하는 방법이 proto::default_domain 
    // 사용하는 것

// Implement the two expression wrappers
template<typename E>
struct spirit_expr
  : proto::extends<E, spirit_expr<E>, spirit_domain>
{
    spirit_expr(E const &e = E()) : spirit_expr::proto_extends(e) {}
};

template<typename E>
struct phoenix_expr
  : proto::extends<E, phoenix_expr<E>, phoenix_domain>
{
    phoenix_expr(E const &e = E()) : phoenix_expr::proto_extends(e) {}
};

int main()
{
    proto::literal<int, spirit_domain> sp(0);
    proto::literal<int, phoenix_domain> phx(0);

    // Whoops! What does it mean to add two expressions in different domains?
    sp + phx; // ERROR
}
