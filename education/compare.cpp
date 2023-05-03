#include <boost/lambda/lambda.hpp>
#include <boost/tuple/tuple.hpp>
#include <boost/tuple/tuple_comparison.hpp>

#include <iostream>
#include <iomanip>
#include <string>
#include <vector>
#include <algorithm>

using namespace std;
using namespace boost::lambda;
using namespace boost;

struct Data
{
  Data(string const& node, int id)
    : m_node(node), m_id(id)
  {}

  string m_node;
  int m_id;
};

bool operator<(Data const& lhs, Data const& rhs)
{
  return 
    make_tuple(lhs.m_node.length(), lhs.m_node, lhs.m_id) <
    make_tuple(rhs.m_node.length(), rhs.m_node, rhs.m_id) ;
}

ostream& operator << (ostream& os, Data& data)
{
  os << setw(7) << left << data.m_node << ":" << setw(4) 
     << right   << data.m_id;

  return os;
}

int main()
{
  vector<Data> vs;

  vs.push_back(Data("CAN2", 2));
  vs.push_back(Data("CAN10", 13));
  vs.push_back(Data("CAN10", 10));
  vs.push_back(Data("CAN1", 1));
  vs.push_back(Data("CAN11", 11));

  stable_sort(vs.begin(), vs.end());
  for_each(vs.begin(), vs.end(), cout << _1 << "\n");
}
