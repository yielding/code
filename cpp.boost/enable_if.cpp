#include <boost/type_traits/is_arithmetic.hpp>
#include <boost/type_traits/is_pointer.hpp>
#include <boost/utility/enable_if.hpp>

class test
{
public:
  // A constructor that works for any argument list of size 10
  template< class... T
          , typename boost::enable_if_c< sizeof...( T ) == 10, int >::type = 0
          >
  test( T&&... );

  // A conversion operation that can convert to any arithmetic type
  template< class T
          , typename boost::enable_if< boost::is_arithmetic< T >, int >::type = 0
          >
  operator T() const;

  // A conversion operation that can convert to any pointer type
  template< class T
          , typename boost::enable_if< boost::is_pointer< T >, int >::type = 0
          >
  operator T() const;
};

int main()
{
  // Works
  test test_( 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 );

  // Fails as expected
  // test fail_construction( 1, 2, 3, 4, 5 );

  // Works by calling the conversion operator enabled for arithmetic types
  int arithmetic_object = test_;

  // Works by calling the conversion operator enabled for pointer types
  int* pointer_object = test_;

  // Fails as expected
  // struct {} fail_conversion = test_;
}
