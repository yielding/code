#include <iostream>
#include <vector>
#include <opencv2/opencv.hpp>
#include <opencv2/features2d.hpp>
#include <opencv2/calib3d.hpp>

using namespace std;
using namespace cv;

void print_accuracy(vector<char>& mask)
{
  int sum = 0;
  for (auto& c: mask) sum += int(c);
  cout << "sum: "   << sum << " total: " << mask.size() << endl;
  cout << "accuracy: " << double(sum) / mask.size() << endl;
}

bool find_homography()
{
  auto src1 = imread("taekwon_v.jpg", IMREAD_GRAYSCALE);
  auto src2 = imread("taekwonv1.jpg", IMREAD_GRAYSCALE);

  auto detector = ORB::create();

  vector<KeyPoint> kp1, kp2;
  Mat desc1, desc2;
  detector->detectAndCompute(src1, Mat(), kp1, desc1);
  detector->detectAndCompute(src2, Mat(), kp2, desc2);

  auto matcher = BFMatcher::create(NORM_HAMMING, true);

  vector<DMatch> matches;
  matcher->match(desc1, desc2, matches);
  sort(matches.begin(), matches.end());
  vector<DMatch> good_matches(matches.begin(), matches.end());

  cout << "number of kp1 : " << kp1.size() << endl;
  cout << "number of kp2 : " << kp2.size() << endl;
  cout << "number of good matches : " << good_matches.size() << endl;

  Mat dst;
  cv::drawMatches(src1, kp1, src2, kp2, good_matches, dst,
      Scalar::all(-1), Scalar::all(-1), vector<char>(), 
      DrawMatchesFlags::NOT_DRAW_SINGLE_POINTS);

  vector<Point2f> src_pts, dst_pts;
  for (auto& m: good_matches)
  {
    src_pts.push_back(kp1[m.queryIdx].pt);
    dst_pts.push_back(kp2[m.trainIdx].pt);
  }

  vector<char> mask;
  auto H = findHomography(src_pts, dst_pts, RANSAC, 5.0, mask);
  print_accuracy(mask);

  vector<Point2f> corners1, corners2;
  corners1.push_back(Point2f(0, 0));
  corners1.push_back(Point2f(src1.cols - 1.f, 0));
  corners1.push_back(Point2f(src1.cols - 1.f, src1.rows - 1.f));
  corners1.push_back(Point2f(0, src1.rows - 1.f));
  perspectiveTransform(corners1, corners2, H);

  vector<Point> corners_dst;
  for (auto& pt: corners2)
    corners_dst.emplace_back(cvRound(pt.x + src1.cols), cvRound(pt.y));

  polylines(dst, corners_dst, true, Scalar(0, 255, 0), 3, LINE_AA);

  Mat dst2;
  drawMatches(src1, kp1, src2, kp2, good_matches, dst2,
      Scalar::all(-1), Scalar::all(-1), 
      mask,
      DrawMatchesFlags::NOT_DRAW_SINGLE_POINTS);

  //imshow("src1", src1);
  //imshow("src2", src2);
  imshow("dst", dst);
  imshow("dst2", dst2);
  waitKey();
  destroyAllWindows();

  return true;
}

int main(int argc, char* argv[])
{
  auto res = find_homography();
  cout << res;

  return 0;
}
