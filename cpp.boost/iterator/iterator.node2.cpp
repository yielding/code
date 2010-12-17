#include "iterator.node2.hpp"

#include <string>
#include <memory>
#include <iostream>
#include <algorithm>
#include <cassert>

#include <boost/mem_fn.hpp>

using namespace std;

ostream_iterator<node_base> out(cout, " ");

int main()
{
    auto_ptr< node<int> > nodes(new node<int>(42));
    nodes->append(new node<string>(" is greater than "));
    nodes->append(new node<int>(13));

    // Check interoperability
    assert(node_iterator(nodes.get()) == node_const_iterator(nodes.get()));
    assert(node_const_iterator(nodes.get()) == node_iterator(nodes.get()));
    
    assert(node_iterator(nodes.get()) != node_const_iterator());
    assert(node_const_iterator(nodes.get()) != node_iterator());
    
    copy(node_iterator(nodes.get()), node_iterator(), out);
    cout << endl;
    
    for_each(node_iterator(nodes.get()), node_iterator(), 
             boost::mem_fn(&node_base::double_me)
    );

    copy(node_const_iterator(nodes.get()), node_const_iterator(), out);
    cout << endl;
    return 0;
}
