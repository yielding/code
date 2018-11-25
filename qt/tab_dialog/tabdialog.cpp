#include "tabdialog.h"

#include <QtWidgets>

////////////////////////////////////////////////////////////////////////////////
//
// Main Dialog
//
////////////////////////////////////////////////////////////////////////////////
TabDialog::TabDialog(const QString& fileName, QWidget *parent)
    : QDialog(parent)
{
  QFileInfo fileInfo(fileName);
  tab = new QTabWidget;
  tab->addTab(new GeneralTab(fileInfo), tr("General"));
  tab->addTab(new PermissionTab(fileInfo), tr("Permission"));

  buttonBox = new QDialogButtonBox(
      QDialogButtonBox::Ok | 
      QDialogButtonBox::Cancel);

  this->connect(buttonBox, &QDialogButtonBox::accepted, this, &QDialog::accept);
  this->connect(buttonBox, &QDialogButtonBox::rejected, this, &QDialog::reject);

  auto mainLayout = new QVBoxLayout;
  mainLayout->addWidget(tab);
  mainLayout->addWidget(buttonBox);
  this->setLayout(mainLayout);
  this->setWindowTitle("탭 다이얼로그");
}

TabDialog::~TabDialog()
{
}

////////////////////////////////////////////////////////////////////////////////
//
// GeneralTab
//
////////////////////////////////////////////////////////////////////////////////
GeneralTab::GeneralTab(const QFileInfo& fileInfo, QWidget* parent)
{
  auto fileNameLabel = new QLabel(tr("File Name:"));
  auto fileNameEdit  = new QLineEdit(fileInfo.fileName());

  auto pathLabel = new QLabel(tr("Path:"));
  auto pathValueLabel = new QLabel(fileInfo.absoluteFilePath());
  pathValueLabel->setFrameStyle(QFrame::Panel | QFrame::Sunken);

  auto sizeLabel = new QLabel(tr("Size:"));
  auto size = fileInfo.size() / 1024;
  auto sizeValueLabel = new QLabel(tr("%1 K").arg(size));
  sizeValueLabel->setFrameStyle(QFrame::Panel | QFrame::Sunken);

  auto lastReadLabel = new QLabel(tr("Last Read:"));
  auto lastReadValueLabel = new QLabel(fileInfo.lastRead().toString());
  lastReadValueLabel->setFrameStyle(QFrame::Panel | QFrame::Sunken);

  auto lastModLabel = new QLabel(tr("Last Modified:"));
  auto lastModValueLabel = new QLabel(fileInfo.lastModified().toString());
  lastModValueLabel->setFrameStyle(QFrame::Panel | QFrame::Sunken);

  auto layout = new QVBoxLayout;
  layout->addWidget(fileNameLabel);
  layout->addWidget(fileNameEdit);
  layout->addWidget(pathLabel);
  layout->addWidget(pathValueLabel);
  layout->addWidget(sizeLabel);
  layout->addWidget(sizeValueLabel);
  layout->addWidget(lastReadLabel);
  layout->addWidget(lastReadValueLabel);
  layout->addWidget(lastModLabel);
  layout->addWidget(lastModValueLabel);

  layout->addStretch(1);

  this->setLayout(layout);
}

////////////////////////////////////////////////////////////////////////////////
//
// GeneralTab
//
////////////////////////////////////////////////////////////////////////////////
PermissionTab::PermissionTab(const QFileInfo& fileInfo, QWidget* parent)
{
}
