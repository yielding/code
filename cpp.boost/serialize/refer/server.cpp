/**
 * OpenCV video streaming over TCP/IP
 * Server: Captures video from a webcam and send it to a client
 * by Isaac Maia
 * 
 * modified by Sheriff Olaoye (sheriffolaoye.com)
 */

#include "opencv2/opencv.hpp"
#include <iostream>
#include <sys/socket.h> 
#include <arpa/inet.h>
#include <sys/ioctl.h>
#include <net/if.h>
#include <unistd.h> 
#include <string.h>

#include <semaphore.h>

using namespace cv;

void *send_data(void *);
void *recv_data(void *);

sem_t semaphore;
bool running = false;
int QUIT = 0;
int capDev = 0;
// open the default camera
VideoCapture cap(capDev);

int main(int argc, char** argv)
{   
  // initialize semaphore;
  sem_init(&semaphore, 0, 1);

  //--------------------------------------------------------
  //networking stuff: socket, bind, listen
  //--------------------------------------------------------
  int localSocket,
      remoteSocket,
      port = 4097;                               
  struct  sockaddr_in localAddr,
                      remoteAddr;
  int addrLen = sizeof(struct sockaddr_in);

  pthread_t send_thread;
  pthread_t recv_thread;

  if ((argc > 1) && (strcmp(argv[1],"-h") == 0)) 
  {
    std::cerr << "usage: ./cv_video_srv [port] [capture device]\n" <<
      "port           : socket port (4097 default)\n" <<
      "capture device : (0 default)\n" << std::endl;

    exit(1);
  }

  if (argc == 2) port = atoi(argv[1]);

  localSocket = socket(AF_INET , SOCK_STREAM , 0);
  if (localSocket == -1)
  {
    perror("socket() call failed!!");
  }    

  localAddr.sin_family = AF_INET;
  localAddr.sin_addr.s_addr = INADDR_ANY;
  localAddr.sin_port = htons( port );

  if (bind(localSocket,(struct sockaddr *)&localAddr , sizeof(localAddr)) < 0) 
  {
    perror("Can't bind() socket");
    exit(1);
  }

  // Listening
  listen(localSocket , 3);

  std::cout <<  "Waiting for connections...\n"
    <<  "Server Port:" << port << std::endl;

  //accept connection from an incoming client
  while(1)
  {
    remoteSocket = accept(localSocket, (struct sockaddr *)&remoteAddr, (socklen_t*)&addrLen);  

    if (remoteSocket < 0) {
      perror("accept failed!");
      exit(1);
    } 

    std::cout << "Connection accepted" << std::endl;
    pthread_create(&send_thread, NULL, send_data, &remoteSocket);
    pthread_create(&recv_thread, NULL, recv_data, &remoteSocket);

    // pthread_join(thread_id, NULL);
  }

  // wait for threads to finish up
  pthread_join(send_thread, NULL);
  pthread_join(recv_thread, NULL);

  // close the socket
  close(remoteSocket);

  return 0;
}

void *send_data(void *ptr)
{
  int socket = *(int *)ptr;
  //OpenCV Code
  //----------------------------------------------------------

  Mat img, flippedFrame;

  int height = cap.get(CAP_PROP_FRAME_HEIGHT);
  int width  = cap.get(CAP_PROP_FRAME_WIDTH);

  std::cout << "height: " << height << std::endl;
  std::cout << "width: " << width << std::endl;

  img = Mat::zeros(height, width, CV_8UC3);

  int imgSize = img.total() * img.elemSize();
  int bytes = 0;

  // make img continuos
  if (!img.isContinuous())
  {
    img = img.clone();
  }

  std::cout << "Image Size:" << imgSize << std::endl;

  bool status = true;

  while (status)
  {
    // get a frame from the camera
    cap >> img;
    // flip the frame
    flip(img, flippedFrame, 1);

    // send the flipped frame over the network
    if ((bytes = send(socket, flippedFrame.data, imgSize, 0)) < 0)
    {
      std::cerr << "bytes = " << bytes << std::endl;
      break;
    }

    sem_wait(&semaphore);
    if(running) status = false;
    sem_post(&semaphore);
  }

  std::cerr << "send quitting..." << std::endl;
  close(socket);
}

void* recv_data(void *ptr)
{
  int socket = *(int *)ptr;
  char *buffer;
  int bytes;

  while(1)
  {
    bytes = recv(socket, buffer, sizeof(int), MSG_WAITALL);

    if (bytes < 0)
    {
      std::cerr << "error receiving from client" << std::endl;
    }
    else if (bytes > 0)
    {
      int msg = atoi(buffer);

      if (msg == QUIT){
        sem_wait(&semaphore);
        running = true;
        sem_post(&semaphore);

        break;
      }
    }
  }

  std::cerr << "recv quitting..." << std::endl;
  close(socket);
}
