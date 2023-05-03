#include "stdafx.h"
#include "signal_cache.h"

#include <boost/multi_index_container.hpp>
#include <boost/multi_index/member.hpp>
#include <boost/multi_index/ordered_index.hpp>
#include <boost/thread/mutex.hpp>

#include <boost/date_time/posix_time/posix_time.hpp>
#include <boost/date_time/gregorian/gregorian.hpp>

using namespace boost::multi_index;
using namespace boost::posix_time;
using namespace boost::gregorian;

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
struct tid {};
struct rid {};
struct time_id {};

typedef boost::multi_index_container <
  RFSignal,
  indexed_by <
    ordered_unique    < tag<tid>,     member<RFSignal, int, &RFSignal::m_tid>  >,
    ordered_non_unique< tag<rid>,     member<RFSignal, int, &RFSignal::m_rid>  >,
    ordered_non_unique< tag<time_id>, member<RFSignal, int, &RFSignal::m_time> >
  >
> RFSignalMap;

typedef RFSignalMap::index<tid>::type cache_tid_index;
typedef RFSignalMap::index<rid>::type cache_rid_index;
typedef RFSignalMap::index<time_id>::type cache_time_index;

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
class RFSignalCacheImpl
{
public:
  RFSignalCacheImpl()
    : m_tid_index(m_cache.get<tid>())
    , m_rid_index(m_cache.get<rid>())
    , m_time_index(m_cache.get<time_id>())
  {}

  int cache_within_duration(RFSignal& signal, int duration)
  {
    boost::mutex::scoped_lock scoped_lock(m_mutex);

    cache_tid_index::iterator it = m_tid_index.find(signal.m_tid);
    signal.m_time = now_to_seconds();
    if (it == m_tid_index.end())
    {
      m_cache.insert(signal);
      return RFSignalCache::cache_new;
    }

    RFSignal tmp = *it;
    tmp.m_tid    = signal.m_tid;
    tmp.m_value  = signal.m_value;
    tmp.m_rid    = signal.m_rid;
    tmp.m_rssi   = signal.m_rssi;
    tmp.m_value  = signal.m_value;
    tmp.m_status = signal.m_status;

    if (tmp.m_time + duration >= signal.m_time)
    {
      m_tid_index.replace(it, tmp);

      return RFSignalCache::cache_discard;
    }

    tmp.m_time = signal.m_time;
    m_tid_index.replace(it, tmp);

    return RFSignalCache::cache_update;
  }

  int cache(RFSignal& signal)
  {
    boost::mutex::scoped_lock scoped_lock(m_mutex);

    cache_tid_index::iterator it = m_tid_index.find(signal.m_tid);
    signal.m_time = now_to_seconds();
    if (it == m_tid_index.end())
    {
      m_cache.insert(signal);
      return RFSignalCache::cache_new;
    }                  

    RFSignal tmp = *it;

    tmp.m_time   = signal.m_time;
    tmp.m_tid    = signal.m_tid;
    tmp.m_rid    = signal.m_rid;
    tmp.m_rssi   = signal.m_rssi;
    tmp.m_value  = signal.m_value;
    tmp.m_status = signal.m_status;

    m_tid_index.replace(it, tmp);

    return RFSignalCache::cache_update;
  } 

  RFSignals get_signals_within(int seconds)
  {                            
    boost::mutex::scoped_lock scoped_lock(m_mutex);

    RFSignals signals;
    int now = now_to_seconds();
    cache_time_index::iterator it0 = m_time_index.lower_bound(now-seconds);
    cache_time_index::iterator it1 = m_time_index.upper_bound(now);
    while (it0 != it1)
      signals.push_back(*it0++);

    return signals;
  }        

  RFSignals get_signals_in_reader(int rid)
  {
    boost::mutex::scoped_lock scoped_lock(m_mutex);

    RFSignals signals;
    cache_rid_index::iterator it = m_rid_index.find(rid);
    while ((*it).m_rid == rid)
      signals.push_back(*it++);

    return signals;
  }

  void clear()
  {
    boost::mutex::scoped_lock scoped_lock(m_mutex);
    m_cache.clear();
  }

  size_t count()
  {
    boost::mutex::scoped_lock scoped_lock(m_mutex);
    return m_cache.size();
  }

private:
  // TODO REMARK
  // 실제 한국시간과는 어느정도 차이가 있다.
  long now_to_seconds()
  {
    ptime beg(date(1970,Jan,1));
    ptime now(second_clock::local_time());

    time_duration diff = now - beg;
    return diff.total_seconds();
  }

private:
  RFSignalMap m_cache;  
  cache_tid_index&  m_tid_index;
  cache_rid_index&  m_rid_index;
  cache_time_index& m_time_index;
  boost::mutex m_mutex;
};

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
RFSignalCache::RFSignalCache()
  :m_impl(new RFSignalCacheImpl)
{}

RFSignalCache::~RFSignalCache()
{}

int RFSignalCache::cache_within_duration(int rid, int tid, int value, int rssi, int status, int duration)
{
  RFSignal signal(rid, tid, value, rssi, status);
  return m_impl->cache_within_duration(signal, duration);
} 

int RFSignalCache::cache(int rid, int tid, int rssi, int value, int status)
{
  RFSignal signal(rid, tid, rssi, value, status);
  return m_impl->cache(signal);
} 
  
RFSignals RFSignalCache::get_signals_within(int sec)
{                            
  return m_impl->get_signals_within(sec);
}        

RFSignals RFSignalCache::get_signals_in_reader(int rid)
{
  return m_impl->get_signals_in_reader(rid);
}        

void RFSignalCache::clear()
{
  return m_impl->clear();
}

size_t RFSignalCache::count()
{
  return m_impl->count();
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
