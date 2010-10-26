#include <netinet/in.h>
#include <cstdlib>
#include <cstring>
#include <cstdio>

void network(unsigned char* no, unsigned int src)
{
  printf("network: ");
  src = htonl(src);
  memcpy((void*)no, (void*)&src, 4); 
  for (int i=0; i<4; i++) 
    printf("%d ", no[i]);

  printf("\n");
}

void host(unsigned char* no, unsigned int src)
{
  printf("host   : ");
  memcpy((void*)no, (void*)&src, 4); 
  for (int i=0; i<4; i++) 
    printf("%d ", no[i]);

  printf("\n");
}

int main()
{
  unsigned char no[4] = { 0 };

  unsigned int a = 0x0123;
  host(no, a);
  network(no, a);
}
