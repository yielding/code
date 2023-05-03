#include "data_model.h"

#include <boost/lexical_cast.hpp>
#include <boost/format.hpp>
#include <boost/algorithm/string.hpp>

#include <algorithm>
#include <iostream>

using namespace boost;
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
bool use_alias(string alias)
{
  cout << alias << endl;

#ifdef WIN32
  return mysql_worker::instance()->connect("192.168.10.110", "openeye", "root", "");
#else
  return mysql_worker::instance()->connect("localhost", "openeye", "root", "");
#endif
  
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
RFSignal::RFSignal(Row const& r)
{
  this->id  = r["id"];
  this->tid = r["tid"];
  this->rid = r["rid"];
  this->rssi = r["rssi"];
  this->signal_time = string(r["signal_time"]);
  this->status = r["status"];
}

RFSignal RFSignal::create(int tid, int rid, int rssi, string dt, int status)
{
  string sql(str(format(
    "insert into rfsignals (tid, rid, rssi, signal_time, status) \
     values(%d, %d, %d, '%s', %d)"
    ) % tid % rid % rssi % dt % status));

  RFSignal s; CONN;
  if (!db->execute(sql, rs))
  {
    cout << "fail...\n";
    return s;
  }

  s.tid = tid;
  s.rid = rid;
  s.rssi = rssi;
  s.signal_time = dt;
  s.status = status;
  // s.id = db->insert_id();

  return s;
}

RFSignals RFSignal::find_by_tid(int tid)
{
  return RFSignal::find_all_impl(
      str(format("CALL sp_rfsignal_select_by_tid(%d)") % tid));
}

RFSignals RFSignal::recent(size_t count)
{
  return RFSignal::find_all_impl(
      str(format("select * from rfsignals order by id limit 0, %d") % count));
}

RFSignals RFSignal::find_all()
{
  return RFSignal::find_all_impl("select * from rfsignals");
  //return RFSignal::find_all_impl("CALL sp_rfsignal_select_all()");
}

RFSignals RFSignal::find_by_tid_rid(int tid, int rid)
{
  return RFSignal::find_all_impl(
      str(format("select * from rfsignals where tid=%d and rfid=%d") % tid % rid));
}

RFSignal RFSignal::find(int id)
{
  return RFSignal::find_impl(str(format("CALL sp_rfsignal_select_by_id(%d)") % id));
}

void RFSignal::delete_all()
{
  CONN; 
  if (!db->execute("delete from rfsignals", rs)) 
    cout << "RFSignal::delete_all fail...\n";
}

void RFSignal::delete_(int id)
{
  CONN; 
  if (!db->execute(str(format("delete from rfsignals where id = %d") % id), rs)) 
    cout << "RFSignal::delete fail...\n";
}

void RFSignal::delete_by_tid(int tid)
{
  CONN; 
  if (!db->execute(str(format("delete from rfsignals where tid = %d") % tid), rs)) 
    cout << "RFSignal::delete_by_tid fail...\n";
}

void RFSignal::remove()
{
  RFSignal::delete_(id);
  id = -1;
}

bool RFSignal::save()
{
  string sql(str(format("CALL sp_rfsignal_save(%d, %d, %d, %d, %d)") 
    % id % tid % rid % rssi % status));

  CONN;
  if (!db->execute(sql, rs))
  {
    cout << "RFSignal::save fail...\n";
    return false;
  }

  return true;
}

string RFSignal::to_s()
{
  return str(format("%d  %d  %d  %s  %d") % id % tid % rid % signal_time % status);
}

Student RFSignal::student()
{
  return Student::find(tid);
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
Parent::Parent(Row const& r)
{
  this->id = r["id"];
  this->name = string(r["name"]);
  this->phone_number = string(r["phone_number"]);
  this->email = string(r["email"]);
}

Parent Parent::find(int id)
{
  return Parent::find_impl(str(format("select * from parents where id = %d") 
        % id));
}

Parents Parent::find_all()
{
  return Parent::find_all_impl("select * from parents");
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
Student::Student(Row const& r)
{
  this->id = r["id"];
  this->pid = r["pid"];
  this->name = string(r["name"]);
}

Parent Student::parent()
{
  return Parent::find(pid);
}

Student Student::find(int id)
{
  return Student::find_impl(str(format("select * from students where id = %d") 
        % id));
}

Students Student::find_all()
{
  return Student::find_all_impl("select * from students");
}

bool Student::have_lunch()
{
  return lunch_time() > 0;
}

int Student::lunch_time()
{
  int rid = 2;
  string sql(str(format(
    "select substr(signal_time,11, 6) from rfsignals "
    "where tid=%d and rid=%d and left(signal_time, 10) = left(now(), 10) "
    "order by signal_time asc"
    ) % id % rid));

  CONN; 
  if (!db->execute(sql, rs))
  {
    cout << "Student::lunch_time fail";
    return -1;
  }

  size_t size = rs.size();
  if (size < 2)
    return -1;

  vector<string> times;
  for (size_t i=0; i<size; i++)
    times.push_back(string(rs[i][0]));

  vector<string> t1; boost::split(t1, times[0], boost::is_any_of(":"));
  int m1 = atoi(t1[0].c_str()) * 60 + atoi(t1[1].c_str());

  vector<string> t2; boost::split(t2, times[size-1], boost::is_any_of(":"));
  int m2 = atoi(t2[0].c_str()) * 60 + atoi(t2[1].c_str());

  return m2 - m1;;
}

string Student::to_s()
{
  return str(format("%d  %d  %d ") % id % pid % name);
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
Reader::Reader(Row const& r)
{
  this->id = r["id"];
  this->name = string(r["name"]);
  this->location = string(r["location"]);
}

Readers Reader::find_all()
{
  return Reader::find_all_impl("select * from readers");
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
