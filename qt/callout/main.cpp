#include <QtWidgets/QApplication>
#include "view.h"

#include <vector>

using namespace std;

int main(int argc, char *argv[])
{
  QApplication a(argc, argv);
  View w;
  w.show();

  return a.exec();
}
