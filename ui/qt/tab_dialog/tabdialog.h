#ifndef TABDIALOG_H
#define TABDIALOG_H

#include <QDialog>

QT_BEGIN_NAMESPACE
class QTabWidget;
class QDialogButtonBox;
class QFileInfo;
QT_END_NAMESPACE

class GeneralTab: public QWidget
{
  Q_OBJECT

public:
    GeneralTab(const QFileInfo& fileInfo, QWidget* parent = nullptr);
};

class PermissionTab: public QWidget
{
  Q_OBJECT

public:
    PermissionTab(const QFileInfo& fileInfo, QWidget* parent = nullptr);
};

class ApplicationTab: public QWidget
{
  Q_OBJECT

public:
    ApplicationTab(const QFileInfo& fileInfo, QWidget* parent = nullptr);
};

////////////////////////////////////////////////////////////////////////////////
//
////////////////////////////////////////////////////////////////////////////////
class TabDialog : public QDialog
{
  Q_OBJECT

public:
  TabDialog(const QString& fileName, QWidget *parent = 0);
  ~TabDialog();
  
public:
  void handleOk();

private:
  QDialogButtonBox
    *buttonBox;

  QTabWidget 
    *tab;
};

#endif // TABDIALOG_H
