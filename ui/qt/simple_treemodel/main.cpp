#include "treemodel.h"

#include <QApplication>
#include <QFile>
#include <QTreeView>

int main(int argc, char *argv[])
{
  Q_INIT_RESOURCE(simple_treemodel);

  QApplication a(argc, argv);

  QFile file(":/default.txt");
  file.open(QIODevice::ReadOnly);

  TreeModel model(file.readAll());
  file.close();

  QTreeView view;
  view.setModel(&model);
  view.setWindowTitle(QObject::tr("Simple Tree Model"));
  view.show();

  return a.exec();
}
