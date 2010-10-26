#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h> /* contains IP #defines */
#include <arpa/inet.h>
#include <errno.h>
#include <stdlib.h>
#include <stdio.h>

#define PORT    30001
#define GROUP   "239.255.0.1"

int main()
{
  int s, len, nbytes, one = 1;
  static struct sockaddr_in sin;
  static struct ip_mreq imr;
  char  buf[100];

  if ((s = socket (AF_INET, SOCK_DGRAM, 0)) == -1)
  {
    perror("socket");
    return 1;
  }

  if (setsockopt (s, SOL_SOCKET, SO_REUSEADDR, (char *) &one, sizeof (one)) == -1)
  {
    perror("setsockopt: SO_REUSEADDR");
    exit(1);
  }

#ifdef SO_REUSEPORT
  /*
   * This must be added for OSF1 v2.x (and BSD 4.3)
   */
  if (setsockopt (s, SOL_SOCKET, SO_REUSEPORT, (char *) &one, sizeof (one)) == -1) 
  {
    perror("setsockopt: SO_REUSEADDR");
    exit(1);
  }
#endif

  sin.sin_family = AF_INET;
  sin.sin_port   = htons (PORT);
  sin.sin_addr.s_addr = htonl (INADDR_ANY);
  if (bind (s, (struct sockaddr *) & sin, sizeof (sin)) == -1) 
  {
    perror ("bind");
    return 1;
  }

  /*
   * the original posting was = htonl(inet_addr (GROUP)) 
   * which is wrong.
   *
   * Send greeting message to multicast group: 
   */
  sin.sin_addr.s_addr = inet_addr (GROUP);
  if (sendto (s, "Hi!", 4, 0, (struct sockaddr *) & sin, sizeof (sin)) == -1) 
  {
    perror("socket");
    return 1;
  }

  /*
   * Join the group: 
   * IP multicast address of group:
   * local IP address of interface:
   */ 
  imr.imr_multiaddr = sin.sin_addr;
  imr.imr_interface.s_addr = htonl (INADDR_ANY);

  if (setsockopt (s, IPPROTO_IP, IP_ADD_MEMBERSHIP, (char *) &imr, sizeof (imr)) == -1)
  {
    /*
     * The original posting did not include this:
     * landmark@cs.tu-berlin.de (Torsten Kerschat) stated:
     * Using setsockopt again with the same options on the
     * same socket would fail, although it is correct. Therefore:
     */
    if (errno != EADDRINUSE)
    {
      perror("setsockopt: IPPROTO_IP, IP_ADD_MEMBERSHIP");
      return 1;
    }

  }

  /*
   * Listen for greeting messages sent to the multicast group: 
   */
  while (1)
  {
    len = sizeof (sin);
    if ((nbytes = recvfrom(s, buf, 100, 0, (struct sockaddr *) &sin, &len)) == -1)
    {
      perror ("recvfrom");
      return 1;
    }

    printf ("%s:\t%.*s\n", inet_ntoa (sin.sin_addr), nbytes, buf);
  }
  /* NOTREACHED */
}
