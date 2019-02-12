#include "boost/lambda/lambda.hpp"
#include "boost/lambda/bind.hpp"
#include "boost/multi_index_container.hpp"
#include "boost/multi_index/member.hpp" 
#include "boost/multi_index/ordered_index.hpp"

#include "boost/format.hpp"
#include <iostream>
#include <algorithm>
#include <string>
#include <map>
#include <functional>
#include <vector>

using boost::format;
using boost::str;

///////////////////////////////////////////////////////////////////////////////
//
//
//
///////////////////////////////////////////////////////////////////////////////
using namespace std;
using namespace boost;
using namespace boost::lambda;

using namespace boost::multi_index;
using boost::multi_index_container;

///////////////////////////////////////////////////////////////////////////////
//
//
//
///////////////////////////////////////////////////////////////////////////////
struct move
{
  move(int const rid, int const tid, int const rssi)
    : m_tid(tid), m_rid(rid), m_rssi(rssi)
  {}

  friend std::ostream& operator<<(std::ostream& os, move const& e)
  {
    os << e.m_rid << " " << e.m_tid << " " << e.m_rssi << "\n";

    return os;
  }

  bool operator<(move const& item)
  {
    return m_tid < item.m_tid;
  }

  int m_rid;
  int m_tid;
  int m_rssi;
};

typedef std::vector<move> move_list;

struct tid{};
struct rid{};

typedef multi_index_container <
  move,
  indexed_by <
    ordered_unique    < tag<tid>, member<move, int, &move::m_tid> >,
    ordered_non_unique< tag<rid>, member<move, int, &move::m_rid> >
  >
> cache_map;

typedef cache_map::index<rid>::type cache_rid_index;
typedef cache_map::index<tid>::type cache_tid_index;

template<typename Tag,typename MultiIndexContainer>
void print_out_by(const MultiIndexContainer& s)
{
  const typename boost::multi_index::index<MultiIndexContainer,Tag>::type& i=
    get<Tag>(s);

  typedef typename MultiIndexContainer::value_type value_type;

  std::copy(i.begin(),i.end(),std::ostream_iterator<value_type>(std::cout));
}

///////////////////////////////////////////////////////////////////////////////
//
//
//
///////////////////////////////////////////////////////////////////////////////
class move_cache
{
public:
  enum { CACHE_NEW, CACHE_UPDATE, CACHE_HIT };

public:
  move_cache()
    : m_tid_index(m_cache.get<tid>()), m_rid_index(m_cache.get<rid>())
  {}

  int cache(move const& item)
  {
    cache_tid_index::iterator it = m_tid_index.find(item.m_tid);

    if (it == m_tid_index.end())
    {
      m_cache.insert(item);
      return CACHE_NEW;
    }

    // the criteria of same is not established yet.
    if (item.m_rid == (*it).m_rid)
      return CACHE_HIT;

    move m = *it;
    m.m_rid = item.m_rid;
    m.m_rssi = item.m_rssi;
    m_tid_index.replace(it, m);

    return CACHE_UPDATE;
  }

  move_list get_items_in_reader(int rid)
  {
    move_list items;
    cache_rid_index::iterator it = m_rid_index.find(rid);

    while ((*it).m_rid == rid)
    {
      items.push_back(*it);
      it++;
    }

    return items;
  }

  void clear()
  {
    m_cache.clear();
  }

private:
  cache_map m_cache;
  cache_tid_index& m_tid_index;
  cache_rid_index& m_rid_index;
};

///////////////////////////////////////////////////////////////////////////////
//
//
//
///////////////////////////////////////////////////////////////////////////////

int main(int argc, char const* argv[])
{
  move_cache ac;

  assert(ac.cache(move(3, 100, 100)) == move_cache::CACHE_NEW);
  assert(ac.cache(move(2, 101, 100)) == move_cache::CACHE_NEW);
  assert(ac.cache(move(2, 102, 100)) == move_cache::CACHE_NEW);
  assert(ac.cache(move(1, 103, 100)) == move_cache::CACHE_NEW);
  assert(ac.cache(move(1, 104, 100)) == move_cache::CACHE_NEW);
  assert(ac.cache(move(1, 105, 100)) == move_cache::CACHE_NEW);
  assert(ac.cache(move(1, 106, 100)) == move_cache::CACHE_NEW);
  assert(ac.cache(move(1, 106,  99)) == move_cache::CACHE_HIT);
  assert(ac.cache(move(2, 106, 100)) == move_cache::CACHE_UPDATE);
  assert(ac.cache(move(2, 103, 100)) == move_cache::CACHE_UPDATE);

  ac.clear();

  move_list const& items1 = ac.get_items_in_reader(1);
  move_list const& items2 = ac.get_items_in_reader(2);
  move_list const& items3 = ac.get_items_in_reader(3);

  for_each(items1.begin(), items1.end(), cout << boost::lambda::_1);
  for_each(items2.begin(), items2.end(), cout << boost::lambda::_1);
  for_each(items3.begin(), items3.end(), cout << boost::lambda::_1);

  // 4. modify with non-unique id -- don't have to implement

  //  아래코드는 사연이 좀 많네. ref(cache), cref(cache)
  // std::for_each(c, c + size, bind(&move_cache::insert, &cache, _1));

  // cout << cache.count();

  return 0;
}

///////////////////////////////////////////////////////////////////////////////
//
//
//
///////////////////////////////////////////////////////////////////////////////
