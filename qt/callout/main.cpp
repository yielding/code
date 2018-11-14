#include <QtWidgets/QApplication>
#include "view.h"

using namespace std;

int main(int argc, char *argv[])
{
  QApplication a(argc, argv);
  View w;
  w.show();
  return a.exec();
}
