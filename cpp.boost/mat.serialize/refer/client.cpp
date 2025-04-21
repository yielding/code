/*
 * OpenCV video streaming over TCP/IP
 * Client: Receives video from server and display it
 * by Steve Tuenkam
 * 
 * modified by Sheriff Olaoye (sheriffolaoye.com)
 */

#include <opencv2/opencv.hpp>
#include <sys/socket.h> 
#include <arpa/inet.h>
#include <unistd.h>

using namespace cv;

int QUIT = 1;

int main(int argc, char** argv)
{
  //--------------------------------------------------------
  //networking stuff: socket , connect
  //--------------------------------------------------------
  int   socket_fd;

  if (argc < 3)
    std::cerr << "Usage: cv_video_cli <serverIP> <serverPort> " << std::endl;

  auto serverIP   = argv[1];
  auto serverPort = atoi(argv[2]);

  sockaddr_in serverAddr;
  auto addrLen = sizeof(struct sockaddr_in);

  if ((socket_fd = socket(PF_INET, SOCK_STREAM, 0)) < 0)
    std::cerr << "socket() failed" << std::endl;

  serverAddr.sin_family = PF_INET;
  serverAddr.sin_addr.s_addr = inet_addr(serverIP);
  serverAddr.sin_port = htons(serverPort);

  if (connect(socket_fd, (sockaddr*)&serverAddr, addrLen) < 0)
    std::cerr << "connect() failed!" << std::endl;

  //----------------------------------------------------------
  // OpenCV Code
  //----------------------------------------------------------

  Mat img;
  img = Mat::zeros(480 , 640, CV_8UC3);
  int imgSize = img.total() * img.elemSize();
  uchar* iptr = img.data;
  int bytes = 0;
  int key = 0;

  //make img continuos
  if (!img.isContinuous())
    img = img.clone();

  std::cout << "Image Size:" << imgSize << std::endl;

  namedWindow("CV Video Client", 1);

  while (key != 'q')
  {
    if ((bytes = recv(socket_fd, iptr, imgSize , MSG_WAITALL)) == -1)
      std::cerr << "recv failed, received bytes = " << bytes << std::endl;

    cv::imshow("CV Video Client", img);
    if ((key = cv::waitKey(10)) >= 0) 
      break;
  }

  // tell server to quit
  send(socket_fd, (void *)QUIT, sizeof(QUIT), 0);

  close(socket_fd);

  return 0;
}
