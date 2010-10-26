#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <errno.h>
#include <cstdio>
#include <cstdlib>
#include <cstring>
#include <netdb.h>
#include <iostream>

using namespace std;

int const MAXLINE   = 500;
char const* ip      = "192.168.10.111";
int const SERV_PORT = 3434;

void dg_cli(int sockfd, char* sendline, sockaddr const* pservaddr, socklen_t servlen)
{
  char recvline[MAXLINE + 1];
  socklen_t	en;
  sockaddr* preply_addr = (sockaddr*)malloc(servlen);

  sendto(sockfd, sendline, strlen(sendline), 0, pservaddr, servlen);

  socklen_t len = servlen;
  int n = recvfrom(sockfd, recvline, MAXLINE, 0, preply_addr, &len);
  cout << "\n" << len << ":" <<  servlen << "\n";
  if (len != servlen || memcmp(pservaddr, preply_addr, len) != 0) 
    printf("error");

  recvline[n] = 0;	//
  printf (recvline);
}

int main(int argc, char **argv)
{
  if (argc <2)
  {
    cout << "too few arg";
    return 1;
  }

  int	sockfd;
  sockaddr_in	servaddr;

  bzero(&servaddr, sizeof(servaddr));
  servaddr.sin_family = AF_INET;
  servaddr.sin_port   = htons(SERV_PORT);
  inet_pton(AF_INET, "192.168.10.108", &servaddr.sin_addr);

  sockfd = socket(AF_INET, SOCK_DGRAM, 0);

  string msg(argv[1]);
  dg_cli(sockfd, argv[1], (sockaddr *) &servaddr, sizeof(servaddr));

  exit(0);
}
