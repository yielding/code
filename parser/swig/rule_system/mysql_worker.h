#include <mysql++.h>

#include <string>

using namespace std;
using namespace mysqlpp;
////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
class mysql_worker
{
public:
  static mysql_worker* instance();
  static void delete_instance();

  ~mysql_worker();

public:
  bool connect(string const& host, string const& db, string const& id, string const& pw);
  bool disconnect();
  bool execute(string const&, StoreQueryResult&);
  bool execute(string const&);
  long insert_id();

private:
  bool execute_impl(string const&, StoreQueryResult&);

private:
  mysql_worker();

  static mysql_worker* m_instance;

  Connection* m_conn;
};

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
