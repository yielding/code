////////////////////////////////////////////////////////////////////////////////
// 
//  Name
//    tafdemo.cpp - OCCI TAF Demo
//  
//  Description
//    Demo of OCCI TAF Feature. 
//    1. Select few records from the emp table in scott user.
//    2. The user is prompted to bounce the server.
//    3. Failover is attempted and the rest of the records
//    are fetched after failover.
// 
////////////////////////////////////////////////////////////////////////////////

#include <occi.h>
#include <cstdio>
#include <iostream>

using namespace oracle::occi;
using namespace std;

int taf_callback(Environment *env, Connection *conn, void *ctx, Connection::FailOverType foType, Connection::FailOverEventType foEvent)
{
  cout << "[Callback] Called " << endl;
  cout << "Failover Event : " ;

  switch (foEvent)
  {
    case Connection::FO_END   : cout << "FO_END"    << endl; break;
    case Connection::FO_ABORT : cout << "FO_ABORT"  << endl; break;
    case Connection::FO_REAUTH: cout << "FO_REAUTH" << endl; break;
    case Connection::FO_BEGIN : cout << "FO_BEGIN"  << endl; break;
    case Connection::FO_ERROR : cout << "FO_ERROR"  << endl; break;
    default                   : cout << "Default "  << endl; break;
  }  

  cout << "Failover Type : " ;
  switch (foType)
  {
    case Connection::FO_NONE   : cout << "FO_NONE"    << endl; break;
    case Connection::FO_SESSION: cout << "FO_SESSION" << endl; break;
    case Connection::FO_SELECT : cout << "FO_SELECT"  << endl; break;
    default                    : cout << "Default "   << endl; break;
  }

  if (foEvent == Connection::FO_ERROR)
    return FO_RETRY;

  if (foEvent == Connection::FO_END)
    cout << "Failover complete" << endl;

  return 0;
}

////////////////////////////////////////////////////////////////////////////////
//
//
//
////////////////////////////////////////////////////////////////////////////////
int main(ub4 argc, char* argv[])
{
  Environment* env;
  Connection* conn;

  try
  {
    // Connect String (database variable) should be
    // failover configured for TAF to work correctly.

    string userName = "SCOTT";
    string password = "TIGER";
    string database = "rnd.solbrain.co.kr/ora";

    // Example of Failover Configured String:  This should be the way
    // the connect string should be configured for failover.
    // mydb      = (DESCRIPTION=(ADDRESS=(PROTOCOL=tcp)(HOST=hostname)\
    // (PORT=portno))(CONNECT_DATA=(SERVICE_NAME=svcname)
    // (FAILOVER_MODE=(TYPE=SELECT)(METHOD=BASIC))))


    // Creating the OCCI Environment and Creating a 
    // connection to the Scott User 
    cout << "OCCI Demo Of TAF" << endl;

    env  = Environment::createEnvironment(Environment::DEFAULT);
    conn = env->createConnection(userName,password,database);
    cout << "OCCI Environment  and Connection Created " << endl;

    // Registering the Connection for TAF Callback.
    cout << "Registering the Connection with the TAF Callback" << endl;
    conn->setTAFNotify(taf_callback, NULL);
  }
  catch (SQLException &e)
  {
    cout << "Error in Constructor Call " << endl;
    cout << "Error Number: " << e.getErrorCode() << endl;
    cout << e.getMessage() << endl;
  }

  try
  {
    string sql("select empno, ename from emp order by empno");
    Statement *stmt = conn->createStatement(sql);

    cout << "Selecting Records from Emp Table " << endl;
    ResultSet *rs = stmt->executeQuery();

    // The logic is as follows.
    // A select statement is issued to the server. After 2 records are
    // fetched the server is bounced. This triggers the failover 
    // and the rest of the records are fetched after the failover
    for (int i=0; i<2; i++)
    {
      rs->next();
      cout <<"Emp No   :" << rs->getInt(1)    << endl;
      cout <<"Emp Name :" << rs->getString(2) << endl;
    }

    cout << "Fetched Partial Number of Records " << endl;
    cout << "Performing Failover" << endl;

    // The callback gets called whenever the failover is 
    // attemped for this connection since the callback is
    // registered with the connection.  
    // Hence if the application tries to fail over even before 
    // the instance is brought up the failover would be attempted 
    // and callback called and FO_ERROR would be raised.
    // In the callback whenever an FO_ERROR is recieved, FO_RETRY is sent. 
    // The failover would be attempted until the instance is 
    // actually brought up which is when FO_END would be signalled 
    // and the failover would succeed

    cout << "Bounce the Server" << endl;
    cout << "Once the Server is brought up. Press any key to continue" << endl;
    fflush(stdin);
    int ch = getchar();

    try
    {
      cout << "Connection About to Fail Over" << endl;
      while (rs->next())
      {
        cout <<"Emp No   :" << rs->getInt(1)    << endl;
        cout <<"Emp No   :" << rs->getString(1)    << endl;
        cout <<"Emp Name :" << rs->getString(2) << endl;
      }

      cout << "Selected All Rows after failover" << endl;
    }
    catch (SQLException &e)
    {
      cout << "Error during Selecting rest of the rows" << endl;
      cout << "Error Number: " << e.getErrorCode() << endl;
      cout << e.getMessage() << endl;
    }

    stmt->closeResultSet(rs);
    conn->terminateStatement(stmt);
  }
  catch (SQLException &e)
  {
    cout << "Exception " << endl;
    cout << "Error Number: " << e.getErrorCode() << endl;
    cout << e.getMessage() << endl;
  }

  // Terminating the Connection and the OCCI Environment
  cout << "Terminating the Connection and The Environment" << endl;

  env->terminateConnection(conn);
  Environment::terminateEnvironment(env);

  cout << "Demo Done" << endl;

}
