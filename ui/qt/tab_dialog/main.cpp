#include "tabdialog.h"
#include <QApplication>

int main(int argc, char *argv[])
{
  QApplication a(argc, argv);
  auto fileName = (argc > 2) ? argv[1] : ".";

  TabDialog w(fileName);
  w.show();

  return a.exec();
}
