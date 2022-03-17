#include <iostream>
#include <opencv2/opencv.hpp>

using namespace std;
using namespace cv;

const char* win = "window";

void onMouseEvent(int evt, int x, int y, int flags, void* param)
{
  Mat& img = *(Mat *)param;

  if (evt == EVENT_LBUTTONDOWN)
  {
    rectangle(img, Rect(x-2, y-2, 4, 4), Scalar(0, 0, 128), -1);
    imshow(win, img);
  }
}

int main(int argc, char* argv[])
{
  Mat img(400, 400, CV_8UC3, Scalar(255, 255, 255));
  namedWindow(win);
  setMouseCallback(win, onMouseEvent, &img);
  imshow(win, img);

  while (true) 
  {
    auto ch = waitKey();
    if (ch == 27) break;
  }

  destroyAllWindows();

  return 0;
}
