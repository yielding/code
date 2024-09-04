#include <signal.h>
#include <unistd.h>
#include <syslog.h>

#include <sys/stat.h>
#include <sys/types.h>

#include <cstdio>
#include <cstdlib>
#include <string>
#include <iostream>

using namespace std;

const int MAXFD = 64;

void daemon_init(const char *pname, int facility) 
{
  if (pid_t pid; (pid = fork()) != 0) 
    exit(0);

  setsid();
  signal(SIGHUP, SIG_IGN);

  chdir("/");

  umask(0);
  for (int i=0; i<MAXFD; i++) close(i);

  openlog(pname, LOG_PID, facility);
}

void daemon_init2()
{
  pid_t pid;
  if ((pid = fork()) != 0) 
    exit(0);

  setsid();
  signal(SIGHUP, SIG_IGN);

  chdir("/");

  umask(0);
  for (int i=0; i<MAXFD; i++) 
    close(i);
}

int main(int argc, char const* argv[])
{
  for (int i = 0; i < 10; ++i) 
  {
    openlog("hi", LOG_PID, LOG_USER);
    sleep(1);
    syslog(LOG_NOTICE, "test.syslog");
    cout << "test.syslog" << endl;
    closelog();
  }
  /*
     if (argc < 2)
     {
     for (;;)
     {
     sleep(1);
     cout << "sleep ...\n";
     }

     return 1;
     }

     if (string(argv[1]) == "daemon")
     {
     daemon_init("program-xxx", LOG_USER);
     while (true)
     {
     sleep(1);
     syslog(LOG_INFO, "test daemon");
     }
     }
     */

  return 0;
}