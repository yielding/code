include "dialog.h"

#include <QApplication>
#include <QDesktopWidget>
#include <QStyleHints>

int main(int argc, char *argv[])
{
  QApplication a(argc, argv);
  QGuiApplication::setApplicationDisplayName(Dialog::tr("Standard Dialog"));

  Dialog dialog;
  auto hints = QGuiApplication::styleHints();
  if (hints->showIsFullScreen() && hints->showIsMaximized())
  {
    auto geometry = QApplication::desktop()->availableGeometry(&dialog);
    dialog.resize(geometry.width() / 3, geometry.height() * 2 / 3);
    dialog.move((geometry.width()  - dialog.width()) / 2,
                (geometry.height() - dialog.height()) / 2);
  }

  dialog.show();

  return a.exec();
}
