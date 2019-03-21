#include "iterator.node1.hpp"

#include <string>
#include <memory>
#include <iostream>
#include <algorithm>
#include <functional>

using namespace std;

ostream_iterator<node_base> out(cout, " ");

int main()
{
  unique_ptr< node<int> > nodes(new node<int>(42));
  nodes->append(new node<string>(" is greater than "));
  nodes->append(new node<int>(13));

  copy(node_iterator(nodes.get()), node_iterator(), out);
  cout << endl;

  for_each(node_iterator(nodes.get()), node_iterator(), 
      [](node_base& it) {it.double_me();});

  copy(node_iterator(nodes.get()), node_iterator(), out);
  cout << std::endl;
}
