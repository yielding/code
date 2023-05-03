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
  tab->addTab(new ApplicationTab(fileInfo), tr("Application"));

  buttonBox = new QDialogButtonBox(
      QDialogButtonBox::Ok | 
      QDialogButtonBox::Cancel);

  // this->connect(buttonBox, &QDialogButtonBox::accepted, this, &QDialog::accept);
  this->connect(buttonBox, &QDialogButtonBox::accepted, this, &TabDialog::handleOk);
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

void TabDialog::handleOk()
{
  // TODO
  this->done(1);
}

/*
MyDialog::MyDialog(QWidget *parent = nullptr) : QDialog(parent)
{
    // Initialize member variable widgets
    m_okButton = new QPushButton("OK", this);
    m_checkBox1 = new QCheckBox("Option 1", this);
    m_checkBox2 = new QCheckBox("Option 2", this);

    // Connect your "OK" button to your custom signal handler
    connect(m_okButton, &QPushButton::clicked, [=]
    {
        int result = 0;
        if (m_checkBox1->isChecked()) {
            // Update result
        }

        // Test other checkboxes and update the result accordingly
        // ...

        // The following line closes the dialog and sets its return value
        this->done(result);            
    });

    // ...
}
 */
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
  auto permissionGroup = new QGroupBox(tr("Permissions"));
  auto readable = new QCheckBox(tr("Readable"));
  if (fileInfo.isReadable())
    readable->setChecked(true);

  auto writable = new QCheckBox(tr("Writable"));
  if (fileInfo.isWritable())
    writable->setChecked(true);

  auto executable = new QCheckBox(tr("Executable"));
  if (fileInfo.isWritable())
    executable->setChecked(true);

  auto ownerLabel = new QLabel(tr("Owner"));
  auto ownerValueLabel = new QLabel(fileInfo.owner());
       ownerValueLabel->setFrameStyle(QFrame::Panel | QFrame::Sunken);

  auto groupLabel = new QLabel(tr("Group"));
  auto groupValueLabel = new QLabel(fileInfo.group());
       groupValueLabel->setFrameStyle(QFrame::Panel | QFrame::Sunken);

  auto permissionLayout = new QVBoxLayout;
  permissionLayout->addWidget(readable);
  permissionLayout->addWidget(writable);
  permissionLayout->addWidget(executable);
  permissionGroup->setLayout(permissionLayout);

  auto ownerGroup = new QGroupBox(tr("Ownership"));
  auto ownerLaout = new QVBoxLayout;
  ownerLaout->addWidget(ownerLabel);
  ownerLaout->addWidget(ownerValueLabel);
  ownerLaout->addWidget(groupLabel);
  ownerLaout->addWidget(groupValueLabel);
  ownerGroup->setLayout(ownerLaout);

  auto layout = new QVBoxLayout();
  layout->addWidget(permissionGroup);
  layout->addWidget(ownerGroup);
  layout->addStretch(1);

  this->setLayout(layout);
}

////////////////////////////////////////////////////////////////////////////////
//
// Application Tab
//
////////////////////////////////////////////////////////////////////////////////
ApplicationTab::ApplicationTab(const QFileInfo& fileInfo, QWidget* parent)
{
  auto topLabel = new QLabel(tr("Open with:"));

  QStringList apps;
  for (int i = 0; i < 30; ++i) 
    apps.append(tr("Application %1").arg(i));
  
  auto appListBox = new QListWidget;
  appListBox->insertItems(0, apps);

  auto alwaysCheckBox = fileInfo.suffix().isEmpty()
    ? new QCheckBox(tr("Always use this app to open this type of file"))
    : new QCheckBox(tr("Always use this app to open files with the rxtension '%1'").arg(fileInfo.suffix()));

  auto layout = new QVBoxLayout;
  layout->addWidget(topLabel);
  layout->addWidget(appListBox);
  layout->addWidget(alwaysCheckBox);
  this->setLayout(layout);
}
