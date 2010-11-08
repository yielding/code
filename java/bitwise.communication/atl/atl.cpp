#include "stdafx.h"
#include <windows.h>
#include <iostream>

using namespace std;

enum PacketType {
  XLL_ERROR       = 0,
  XLL_HELLO       = 1,
  XLL_BYE         = 2,
  XLL_PING        = 3,            // 중요하겠죠?
  XLL_CREQ_ALL    = 4,
  XLL_CREQ_TABLE  = 5,
  XLL_CREQ_EXPORT = 6,
  XLL_BREQ_ALL    = 7,
  XLL_BREQ_TABLE  = 8,
  XLL_BREQ_EXPORT = 9,
  XLL_REQ_UNDEF   = 10
  // 기타 다양한 데이타 소스(x.25등등...)
};

struct xLLCommon {
  unsigned int packetSize : 16;
  unsigned int packetType : 8;
  unsigned int message    : 6;
  unsigned int version    : 2;
};

#include <iostream>

using namespace std;

int _tmain(int argc, _TCHAR* argv[])
{
  SOCKET s;

  WSADATA wsaData;
  if (WSAStartup(MAKEWORD(2, 2), &wsaData) != 0) 
  {
    WSACleanup();
    // TODO: where to put below line..
    //return XLL_LIBRARY_NOT_FOUND;
  }

  if ((s = socket(AF_INET, SOCK_STREAM, 0)) == SOCKET_ERROR) 
  {
    return -1;
  }

  SOCKADDR_IN serv_addr;
  serv_addr.sin_family      = AF_INET;
  serv_addr.sin_port        = htons(1010);
  serv_addr.sin_addr.s_addr = inet_addr("127.0.0.1");

  if (::connect(s, (SOCKADDR FAR *)&serv_addr, sizeof(SOCKADDR_IN)) == -1) {
    return -1;
  }

  char buffer1[4] = { 0 };
  xLLCommon *ph = (xLLCommon*)buffer1;
  ph->version    = 1;
  ph->message    = 0;
  ph->packetType = 15;
  ph->packetSize = 1024;

  *((u_long *)buffer1) = htonl(*(u_long*)(buffer1));
  int nBytesSent = ::send(s, (char*)buffer1, 4, 0);
  int nBytesRead = ::recv(s, (char*)buffer1, 4, 0);
  *((u_long *)buffer1) = ntohl(*(u_long*)(buffer1));

  cout << ph->version << " " << ph->message << " " << ph->packetType
    << " " << ph->packetSize << endl;
  return 0;
}
