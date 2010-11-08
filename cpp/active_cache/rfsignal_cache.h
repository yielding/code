#ifndef _SIGNAL_CACHE_H__
#define _SIGNAL_CACHE_H__

#include <boost/shared_ptr.hpp>

#include <vector>
#include <string>

using namespace std;
///////////////////////////////////////////////////////////////////////////////
//
//
//
///////////////////////////////////////////////////////////////////////////////                                  
struct RFSignal
{
  RFSignal()
    : m_rid(0), m_tid(0), m_rssi(0), m_value(0), m_status(0), m_time(0)
 {}

  RFSignal(int rid, int tid, int rssi, int value, int status, int time=0)
    : m_rid(rid), m_tid(tid), m_rssi(rssi), m_value(value)
    , m_status(status)
    , m_time(time)
  {}

  bool operator<(RFSignal const& item) 
  {      
    return m_tid < item.m_tid;
  }

  std::string to_s();

  int m_rid;
  int m_tid;
  int m_rssi;
  int m_value;
  int m_status;
  int m_time;
};    

///////////////////////////////////////////////////////////////////////////////
//
//
//
///////////////////////////////////////////////////////////////////////////////                                  
typedef std::vector<RFSignal> RFSignals;

///////////////////////////////////////////////////////////////////////////////
//
//
//
///////////////////////////////////////////////////////////////////////////////
class RFSignalCacheImpl;

class RFSignalCache
{
public:
  // REMARK: 우리 캐쉬는 항상 시간이 업데이트 되므로 hit == update
  enum { cache_new, cache_update, cache_hit, cache_discard };

public:
  RFSignalCache(int interval=10);
  ~RFSignalCache();

  int       cache_within_duration(int rid, int tgid, int value, int rssi, 
              int status, int duration);
  int       cache(int rid, int tid, int rssi, int value, int status);
  RFSignals get_signals_within(int seconds);
  RFSignals get_signals_in_reader(int rid);
  size_t    count();
  void      clear();
  
public:
  void      start();
  void      stop();

  void           clear_after(int minutes, string const& tn);
  RFSignalCache& clear_at(int mi, int hh, int dd, int mo, int ww, string const& tn);
  RFSignalCache& clear_in(string const& mi, string const& hh, string const& dd, 
    string const& mo, string const& ww, string const& tn);

private:
  boost::shared_ptr<RFSignalCacheImpl> m_impl;
};

///////////////////////////////////////////////////////////////////////////////
//
//
//
///////////////////////////////////////////////////////////////////////////////
#endif
