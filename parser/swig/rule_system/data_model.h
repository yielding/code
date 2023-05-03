#ifndef DATA_MODEL_H__
#define DATA_MODEL_H__

#include "mysql_worker.h"

#include <string>
#include <vector>

using namespace std;
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
#define CONN StoreQueryResult rs; \
             mysql_worker* db = mysql_worker::instance();

bool use_alias(string alias);

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
class Parent;   typedef vector<Parent> Parents;
class Reader;   typedef vector<Reader> Readers;
class Student;  typedef vector<Student> Students;
class RFSignal; typedef vector<RFSignal> RFSignals;

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
template<typename Record>
class Table
{
public:
  typedef vector<Record> Records;

protected:
  static Records find_all_impl(string const& sql)
  {
    CONN; Records r;
    if (!db->execute(sql, rs)) { cout << "find_all.exec fail\n"; return r; }

    int results_no = rs.size();
    for (int i=0; i<results_no; i++)
      r.push_back(Record(rs[i]));

    return r;
  }

  static Record find_impl(string const& sql)
  {
    CONN; Record dummy; 
    if (!db->execute(sql, rs)) { cout << "find.exec fail...\n"; return dummy; }

    int results_no = rs.size();
    if (results_no <= 0) 
      return dummy;

    return Record(rs[0]);
  }

private:
  Record const& self() 
  { 
    return static_cast<Record const&>(*this); 
  }
};

////////////////////////////////////////////////////////////////////////////////
//
// Core class CRTP?
//
////////////////////////////////////////////////////////////////////////////////
class RFSignal: public Table<RFSignal>
{
public:
  RFSignal() { id = -1; }
  RFSignal(Row const& r);

public: // core service function
  static RFSignal  create(int tid, int rid, int rssi, string dt, int status);
  static RFSignal  find(int id);
  static RFSignals find_all();
  static RFSignals find_by_tid(int tid);
  static RFSignals find_by_tid_rid(int tid, int rid);
  static RFSignals recent(size_t count);

  static void delete_(int id);
  static void delete_all();
  static void delete_by_tid(int tid);

  void remove();
  bool save();
  bool ok() { return id != -1; }

public: // application specific function
  Student student();

  string  to_s();

public:
  int tid;
  int rid;
  int rssi;
  int status;
  string signal_time;

private:
  int id;
};

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
class Parent: public Table<Parent>
{
public:
  Parent() { id = -1; }
  Parent(Row const& r);

  static Parent  find(int id);
  static Parents find_all();

  string name;
  string phone_number;
  string email;
  int id;
};

class Student: public Table<Student>
{
public:
  Student() { id = -1; }
  Student(Row const& r);

public:
  static Student  find(int id);
  static Students find_all();

  int    id;
  int    pid; // parent id
  string name;

public:
  Parent parent();
  bool   have_lunch();
  int    lunch_time();
  string to_s();
};

class Reader: public Table<Reader>
{
public:
  Reader() { id = -1; }
  Reader(Row const& r);

public:
  static Readers find_all();

  string name;
  string location;
  int id;
};

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
#endif
