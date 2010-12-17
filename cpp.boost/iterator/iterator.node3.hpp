#ifndef NODE_ITERATOR3_DWA2004110_HPP
# define NODE_ITERATOR3_DWA2004110_HPP

# include "iterator.node.hpp"
# include <boost/iterator/iterator_adaptor.hpp>

# ifndef BOOST_NO_SFINAE
#  include <boost/type_traits/is_convertible.hpp>
#  include <boost/utility/enable_if.hpp>
# endif

template <class Value>
class node_iter
  : public boost::iterator_adaptor<
        node_iter<Value>                // Derived
      , Value*                          // Base
      , boost::use_default              // Value
      , boost::forward_traversal_tag    // CategoryOrTraversal
    >
{
 private:
    struct enabler {};  // a private type avoids misuse

    typedef boost::iterator_adaptor<
        node_iter<Value>, Value*, boost::use_default, boost::forward_traversal_tag
    > super_t;
    
 public:
    node_iter()
      : super_t(0) {}

    explicit node_iter(Value* p)
      : super_t(p) {}

    template <class OtherValue>
    node_iter(
        node_iter<OtherValue> const& other
# ifndef BOOST_NO_SFINAE
      , typename boost::enable_if<
            boost::is_convertible<OtherValue*,Value*>
          , enabler
        >::type = enabler()
# endif 
    )
      : super_t(other.base()) {}

# if !BOOST_WORKAROUND(__GNUC__, == 2)
 private: // GCC2 can't grant friendship to template member functions    
    friend class boost::iterator_core_access;
# endif     
    void increment() { this->base_reference() = this->base()->next(); }
};

typedef node_iter<node_base> node_iterator;
typedef node_iter<node_base const> node_const_iterator;

#endif // NODE_ITERATOR3_DWA2004110_HPP
