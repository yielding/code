#include <opencv2/opencv.hpp>
#include <opencv2/imgproc/imgproc.hpp>
#include <fstream>
#include <iostream>

using namespace std;

cv::VideoCapture g_cap;
int g_dontset = 0, g_run = 1;
int g_slider_position = 0;

void on_trackbar_slide(int pos, void*)
{
  g_cap.set(cv::CAP_PROP_POS_FRAMES, pos);
  if (!g_dontset)
    g_run = 1;

  g_dontset = 0;
}

int main(int argc, char* argv[])
{
  cv::Mat img;
  cv::namedWindow("EXAMPLE2_4", cv::WINDOW_AUTOSIZE);
  g_cap.open("/Users/yielding/Desktop/festa2025.mp4");

  int frames = (int) g_cap.get(cv::CAP_PROP_FRAME_COUNT);
  int tmpw   = (int) g_cap.get(cv::CAP_PROP_FRAME_WIDTH);
  int tmph   = (int) g_cap.get(cv::CAP_PROP_FRAME_HEIGHT);

  cv::createTrackbar("Position", "EXAMPLE2_4", &g_slider_position, frames, on_trackbar_slide);

  cv::Mat frame;

  while (true)
  {
    if (g_run != 0) {
      g_cap >> frame;
      if (frame.empty()) break;
      int current_pos = (int)g_cap.get(cv::CAP_PROP_POS_FRAMES);
      g_dontset = 1;

      cv::setTrackbarPos("Position", "EXAMPLE2_4", current_pos);
      cv::imshow("EXAMPLE2_4", frame);

      g_run -= 1;
    }

    auto c = (char)cv::waitKey(10);
    if (c == 's') {
      g_run = 1;
      cout << "Simple step, run = " << g_run << endl;
    }

    if (c == 'r') {
      g_run = -1;
      cout << "Run mode, run = " << g_run << endl;
    }

    if (c == 27)
      break;
  }

  return 0;
}
