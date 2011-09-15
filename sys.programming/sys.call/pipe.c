#include <unistd.h>
#include <stdio.h>

int main()
{
  int fd[2];

  pipe(fd);
  dup2(fd[0], STDIN_FILENO);
  dup2(fd[1], STDOUT_FILENO);
  close(fd[0]);
  close(fd[1]);

  for (int i=0; i<10; i++)
  {
    int myint;
    write(STDOUT_FILENO, &i, sizeof(i));
    read(STDIN_FILENO, &myint, sizeof(myint));
    /* printf("%d ", i); fflush(stdout); scanf("%d", &myint); */
    fprintf(stderr, "%d\n", myint);
  }

  return 0;
}
