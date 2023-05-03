#include "stdafx.h"

#include "mysql_worker.h"

#include <boost/format.hpp>

#include <iostream>

using namespace boost;
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
namespace
{
  string quote(string s)
  {
    return str(format("%s") % s);
  }

  string join(vector<string>& args)
  {
    if (args.size() == 0)
      return "";

    string ss = quote(args[0]);
    for (size_t i=1; i<args.size(); i++)
    {
      ss += ", " + quote(args[i]);
    }

    return ss;
  }
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
mysql_worker* mysql_worker::m_instance = 0;

mysql_worker* mysql_worker::instance()
{
  if (m_instance == 0)
    m_instance = new mysql_worker();

  return m_instance;
}

void mysql_worker::delete_instance()
{
  if (m_instance != NULL)
  {
    delete m_instance;
    m_instance = NULL;
  }
}

mysql_worker::mysql_worker()
{
  m_conn = NULL;
}

bool mysql_worker::connect(string const& host, string const& db, 
    string const& id, string const& pw)
{
  m_conn = new Connection(true);
  if (m_conn == NULL)
    return false;

  // TODO for SP
  // m_conn->set_option(Connection::opt_multi_statements, true);
  // m_conn->set_option(Connection::opt_reconnect, true);
  bool res = m_conn->connect(db.c_str(), host.c_str(), id.c_str(), pw.c_str() );
  cout << "connection " << (res ? "ok" : "fail") << endl;
  return res;
}

mysql_worker::~mysql_worker()
{
  disconnect();
}

bool mysql_worker::disconnect()
{
  if (m_conn != NULL)
  {
    try
    {
      // TODO
      // m_conn->close();
      delete m_conn;
      m_conn = NULL;
    }
    catch (mysqlpp::Exception const&)
    {
      return false;
    }
  }

  return true;
}

/*
long mysql_worker::insert_id()
{
  if (m_conn == NULL)
    return -1U;

  return m_conn->insert_id();
}
*/

bool mysql_worker::execute(string const& script, StoreQueryResult& rs)
{
  return execute_impl(script, rs);
}

bool mysql_worker::execute(string const& script)
{
  StoreQueryResult rs;
  return execute_impl(script, rs);
}

bool mysql_worker::execute_impl(string const& script, StoreQueryResult& rs)
{
  try
  {
    Query query = m_conn->query(script);
    rs = query.store();

    // for sp to operate correctly
    while (query.more_results()) 
      StoreQueryResult r = query.store_next();

    return true;
  }
  catch(BadQuery const& ex)
  {
    cout << "Bad Query: " << ex.what() << endl;
  }

  return false;
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
