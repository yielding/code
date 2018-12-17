#include <QApplication>
#include <QDesktopWidget>
#include <QCommandLineParser>
#include <QCommandLineOption>
#include <QFileSystemModel>
#include <QFileIconProvider>
#include <QTreeView>

int main(int argc, char *argv[])
{
  QApplication app(argc, argv);
  QCoreApplication::setApplicationVersion(QT_VERSION_STR);

  QCommandLineParser parser;
  parser.setApplicationDescription("Qt Dir View Example");
  parser.addHelpOption();
  parser.addVersionOption();
  QCommandLineOption dontUseCustomDirIconOption("c", "Set QFileIconProvider::DontUseCustomDirectoryIcons");
  parser.addOption(dontUseCustomDirIconOption);
  parser.addPositionalArgument("directory", "The directory to start in.");
  parser.process(app);

  auto root = parser.positionalArguments().isEmpty()
    ? QString()
    : parser.positionalArguments().first();

  QFileSystemModel model;
  model.setRootPath("");
  if (parser.isSet(dontUseCustomDirIconOption))
    model.iconProvider()->setOptions(QFileIconProvider::DontUseCustomDirectoryIcons);

  QTreeView tree;
  tree.setModel(&model);
  if (!root.isEmpty())
  {
    auto rootIndex = model.index(QDir::cleanPath(root));
    if (rootIndex.isValid())
      tree.setRootIndex(rootIndex);
  }

  tree.setAnimated(false);
  tree.setIndentation(25);
  tree.setSortingEnabled(true);

  auto availableSize = QApplication::desktop()->availableGeometry(&tree).size();
  tree.resize(availableSize / 2);
  tree.setColumnWidth(0, tree.width() / 3);
  tree.setWindowTitle(QObject::tr("Dir View"));
  tree.show();


  return app.exec();
}
