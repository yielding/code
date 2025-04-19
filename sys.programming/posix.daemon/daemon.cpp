#include <signal.h>
#include <unistd.h>
#include <syslog.h>
#include <fcntl.h>

#include <sys/stat.h>
#include <sys/types.h>
#include <sys/file.h>

#include <cstdio>
#include <cstdlib>
#include <string>
#include <print>

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

  int fd = open("/var/run/mydaemon.lock", O_CREAT | O_RDWR, 0666);
  if (flock(fd, LOCK_EX | LOCK_NB) == -1) 
  {
    println("already instance is running..");
    exit(1);
  }

  println("daomon successfully launched..");

  openlog(pname, LOG_PID, facility);
}

void daemon_init2()
{
  if (pid_t pid; (pid = fork()) != 0) 
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
  if (argc < 2)
  {
    for (;;)
    {
      sleep(1);
      println("sleep...");  
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

  return 0;
}