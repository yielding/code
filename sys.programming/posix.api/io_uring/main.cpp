#include <print>
#include <cstdio>
#include <fcntl.h>
#include <string.h>
#include <unistd.h>
#include <liburing.h>

#define BUF_SIZE 0x2000

int main() 
{
  // 큐 초기화
  io_uring ring;
  io_uring_queue_init(8, &ring, 0);  

  auto fd = open("example.txt", O_RDONLY);
  if (fd < 0)
  {
    perror("open");
    return 1;
  }

  char buffer[BUF_SIZE] = {0};
  auto sqe = io_uring_get_sqe(&ring);  // SQE 확보
  io_uring_prep_read(sqe, fd, buffer, BUF_SIZE, 0);  // read 준비
  io_uring_submit(&ring);              // 커널에 제출

  io_uring_cqe *cqe;
  io_uring_wait_cqe(&ring, &cqe);      // 완료 대기

  if (cqe->res < 0)
  {
    std::println("Async read failed: {}", strerror(-cqe->res));
  }
  else
  {
    std::println("Read {} bytes:", cqe->res);
    write(STDOUT_FILENO, buffer, cqe->res);  // 읽은 내용 출력
  }

  io_uring_cqe_seen(&ring, cqe);             // CQE 소비
  io_uring_queue_exit(&ring);
  close(fd);

  return 0;
}
