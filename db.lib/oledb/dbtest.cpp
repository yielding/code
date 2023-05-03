// dbtest.cpp : Defines the entry point for the console application.
//

#include "stdafx.h"

#import "C:\Program Files\Common Files\System\ADO\msado20.tlb" \
  no_namespace rename("EOF", "ADOEOF")

static char ConnStr[] = "Provider=Microsoft.Jet.OLEDB.4.0;Data Source=tabssmtps.mdb;";

void PrintComError(_com_error &e)
{
  _bstr_t bstrSource(e.Source());
  _bstr_t bstrDescription(e.Description());

  // Print COM errors.
  printf("Error\r\n");	
  printf("\tCode = %08lx\r\n", e.Error());
  printf("\tCode meaning = %s\r\n", e.ErrorMessage());
  printf("\tSource = %s\r\n", (LPCSTR) bstrSource);
  printf("\tDescription = %s\r\n", (LPCSTR) bstrDescription);
}

void InsertData()
{
  char Query[512];
  _ConnectionPtr pConn;
  _RecordsetPtr pRs = NULL;

  try {
    pConn.CreateInstance(__uuidof(Connection));
    pConn->Open(_bstr_t(ConnStr), _bstr_t(""), _bstr_t(""), adConnectUnspecified); 

    int TotalInsert = 0;

    char datestr[32];
    for (int j = 0; j < 1000; j++) 
    {
      DWORD dwStart = GetTickCount();
      for (int i = 0; i < 1000; i++) 
      {
        wsprintf(datestr, "2004-%02d-%02d %02d:%02d", rand() % 12 + 1, rand() % 29 + 1, rand() % 24, rand() % 60);

        wsprintf(Query, "INSERT INTO SendMsg0(_from, _to, _retry, _mailid, _emlfile, _campaign, _schedule) "
            "VALUES ('%s', '%s', %d, '%s', '%s', '%s', '%s')",
            "khkim@tabslab.com",
            "help@tabslab.com",
            1,
            "8273625574",
            "8273625574.eml",
            "campaign 10",
            datestr
            );

        pConn->BeginTrans();
        pConn->Execute(_bstr_t(Query), NULL, adCmdText);
        pConn->CommitTrans();

        TotalInsert++;
      }

      printf("Insert Count = %d, Elapsed Time = %d sec\n", TotalInsert, (GetTickCount() - dwStart) / 1000);
    }

    pConn->Close();		
  } 
  catch (_com_error& e) 
  {
    pConn->RollbackTrans();
    PrintComError(e);		
  }
}

void DeleteAllData()
{
  _ConnectionPtr pConn;

  try 
  {
    pConn.CreateInstance(__uuidof(Connection));
    pConn->Open(_bstr_t(ConnStr), _bstr_t(""), _bstr_t(""), adConnectUnspecified); 

    pConn->Execute(_bstr_t("DELETE FROM SendMsg0"), NULL, adCmdText);

    pConn->Close();		
  } 
  catch (_com_error& e)
  {
    pConn->RollbackTrans();
    PrintComError(e);		
  }
}

void SelectData()
{
  char Query[512], datestr[32];
  _ConnectionPtr pConn;
  _RecordsetPtr pRs = NULL;

  try 
  {
    pConn.CreateInstance(__uuidof(Connection));
    pConn->Open(_bstr_t(ConnStr), _bstr_t(""), _bstr_t(""), adConnectUnspecified); 

    DWORD dwStart = GetTickCount();
    for (int i = 0; i < 1000; i++) 
    {
      wsprintf(datestr, "2004-%02d-%02d", rand() % 12 + 1, rand() % 29 + 1);
      wsprintf(Query, "SELECT * FROM SendMsg0 WHERE [_schedule] < #%s#", datestr);

      pRs = pConn->Execute(_bstr_t(Query), NULL, adCmdText);

      int Row = 0;
      while (pRs->ADOEOF == VARIANT_FALSE && Row < 100) 
      {
        pRs->MoveNext();
        Row++;
      }
    }
    printf("Select Count = 1000, Elapsed Time = %d sec\n", (GetTickCount() - dwStart) / 1000);

    pRs->Close();

    pConn->Close();
  } catch (_com_error& e) {
    PrintComError(e);
  }
}

int main(int argc, char* argv[])
{
  CoInitialize(NULL);

  //InsertData();	
  //DeleteAllData();
  SelectData();

  CoUninitialize();

  return 0;
}

