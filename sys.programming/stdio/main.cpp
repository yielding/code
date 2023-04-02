#include <iostream>
#include <cstdlib>
#include <libproc.h>
#include <unistd.h>

using namespace std;

string get_process_name(pid_t pid) 
{
  char process_name[PROC_PIDPATHINFO_MAXSIZE];
  int result = proc_name(pid, process_name, sizeof(process_name));
  if (result <= 0)
    return "";

  return string(process_name);
}

int main(int argc, char* argv[])
{
  // Get the PID of the process to get the name of
  // pid_t pid = getpid(); 
  // Replace with the PID of the process you want to get the name of

  if (argc != 2)
  {
    cout << "main pid" << endl;
    exit(EXIT_FAILURE);
  }

  auto pid = atoi(argv[1]);
  // Get the name of the process
  auto process_name = get_process_name(pid);

  cout << " Process " << pid 
       << " is named " << process_name << endl;

  return 0;
}