#include <boost/proto/proto.hpp>
#include <vector>
#include <iostream>

using namespace std;
      namespace proto = boost::proto;

template< typename E >
struct my_expr;

struct my_generator
  : proto::pod_generator<my_expr>
{};

struct my_domain
  : proto::domain<my_generator>
{
     // Make as_child() behave like as_expr() in my_domain.
     // (proto_base_domain is a typedef for proto::domain< my_generator >
     // that is defined in proto::domain<>.)
     template< typename T >
     struct as_child
       : proto_base_domain::as_expr<T>
     {};
};

//template< typename E >
//struct my_expr
//{
//    BOOST_PROTO_EXTENDS( E, my_expr< E >, my_domain )
//};

int main(int argc, char const* argv[])
{
    proto::literal< int, my_domain > i(0);
    auto l = i + 42; // OK! Everything is stored by value here.        


    return 0;
}

