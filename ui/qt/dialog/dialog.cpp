#include "dialog.h"

#include <QtWidgets>

#define MESSAGE \
    Dialog::tr("<p>Message boxes have a caption, a text, " \
               "and any number of buttons, each with standard or custom texts." \
               "<p>Click a button to close the message box. Pressing the Esc button " \
               "will activate the detected escape button (if any).")
#define MESSAGE_DETAILS \
    Dialog::tr("If a message box has detailed text, the user can reveal it " \
               "by pressing the Show Details... button.")

// REMARK
// 항상 표시해야하는 3개의 widget을 보여주는 코드의 중복을 제거하기 위해서 사용
class DialogOptionsWidget: public QGroupBox
{
public:
  explicit DialogOptionsWidget(QWidget* parent = nullptr)
    : QGroupBox(parent), layout(new QVBoxLayout)
  {
    setTitle(Dialog::tr("Options"));
    setLayout(layout);
  }

  void addCheckBox(const QString& text, int value)
  {
    auto checkBox = new QCheckBox(text);
    layout->addWidget(checkBox);
    checkBoxEntries.append(CheckBoxEntry(checkBox, value));
  }

  void addSpacer()
  {
    layout->addItem(new QSpacerItem(0, 0, QSizePolicy::Ignored, QSizePolicy::MinimumExpanding));
  }

  int value() const
  {
    int result = 0;
    for (auto& entry: checkBoxEntries)
    {
      if (entry.first->isChecked())
        result |= entry.second;
    }

    return result;
  }

private:
  typedef QPair<QCheckBox*, int> CheckBoxEntry;
  QVBoxLayout* layout;
  QList<CheckBoxEntry> checkBoxEntries;
};

Dialog::Dialog(QWidget *parent)
  : QWidget(parent)
{
  QVBoxLayout* vLayout;

  auto hints = QGuiApplication::styleHints();
  if (hints->showIsFullScreen() && hints->showIsMaximized())
  {
    auto hLayout  = new QHBoxLayout(this);
    auto groupBox = new QGroupBox(QGuiApplication::applicationDisplayName(), this);
    hLayout->addWidget(groupBox);
    vLayout = new QVBoxLayout(groupBox);
  }
  else
  {
    vLayout = new QVBoxLayout(this);
  }

  auto toolbox = new QToolBox;
  vLayout->addWidget(toolbox);

  errorMessageDlg = new QErrorMessage(this);

  auto frameStyle = QFrame::Sunken | QFrame::Panel;

  integerLabel = new QLabel;
  integerLabel->setFrameStyle(frameStyle);
  auto integerButton = new QPushButton("QInputDialog::getInt()");

  doubleLabel = new QLabel;
  doubleLabel->setFrameStyle(frameStyle);
  auto doubleButton = new QPushButton("QInputDialog::getDouble()");

  itemLabel = new QLabel;
  itemLabel->setFrameStyle(frameStyle);
  auto itemButton = new QPushButton("QInputDialog::getItem()");

  textLabel = new QLabel;
  textLabel->setFrameStyle(frameStyle);
  auto textButton = new QPushButton("QInputDialog::getText()");

  mlTextLabel = new QLabel;
  mlTextLabel->setFrameStyle(frameStyle);
  auto mlTextButton = new QPushButton("QInputDialog::getMultilineText()");

  connect(integerButton, &QAbstractButton::clicked, this, &Dialog::setInteger);
  connect(doubleButton,  &QAbstractButton::clicked, this, &Dialog::setDouble);
  connect(itemButton,    &QAbstractButton::clicked, this, &Dialog::setItem);
  connect(textButton,    &QAbstractButton::clicked, this, &Dialog::setText);
  connect(mlTextButton,  &QAbstractButton::clicked, this, &Dialog::setMultilineText);

  // input dialog page
  auto page   = new QWidget;
  auto layout = new QGridLayout(page);
  layout->setColumnStretch(1, 1);
  layout->setColumnMinimumWidth(1, 250);
  layout->addWidget(integerButton, 0, 0);
  layout->addWidget(integerLabel,  0, 1);
  layout->addWidget(doubleButton,  1, 0);
  layout->addWidget(doubleLabel,   1, 1);
  layout->addWidget(itemButton,    2, 0);
  layout->addWidget(itemLabel,     2, 1);
  layout->addWidget(textButton,    3, 0);
  layout->addWidget(textLabel,     3, 1);
  layout->addWidget(mlTextButton,  4, 0);
  layout->addWidget(mlTextLabel,   4, 1);
  toolbox->addItem(page, tr("Input Dialogs"));

  // color dialog page
  page   = new QWidget;
  layout = new QGridLayout(page);
  colorLabel = new QLabel;
  colorLabel->setFrameStyle(frameStyle);
  auto colorButton = new QPushButton("QColorDialog::getColor()");

  connect(colorButton, &QAbstractButton::clicked, this, &Dialog::setColor);

  layout->setColumnStretch(1, 1);
  layout->addWidget(colorButton, 0, 0);
  layout->addWidget(colorLabel,  0, 1);
  colorDlgOptions = new DialogOptionsWidget;
  colorDlgOptions->addCheckBox("Do not use native dialog", QColorDialog::DontUseNativeDialog);
  colorDlgOptions->addCheckBox("Show alpha channel", QColorDialog::ShowAlphaChannel);
  colorDlgOptions->addCheckBox("No buttons", QColorDialog::NoButtons);
  layout->addItem(new QSpacerItem(0, 0, QSizePolicy::Ignored, QSizePolicy::MinimumExpanding), 1, 0);
  layout->addWidget(colorDlgOptions, 2, 0, 1, 2);
  toolbox->addItem(page, "Color Dialog");

  // font dialog page
  page = new QWidget;
  layout = new QGridLayout(page);
  fontLabel = new QLabel;
  fontLabel->setFrameStyle(frameStyle);
  auto fontButton = new QPushButton("QFontDialog::getFont()");

  connect(fontButton, &QAbstractButton::clicked, this, &Dialog::setFont);

  layout->setColumnStretch(1, 1);
  layout->addWidget(fontButton, 0, 0);
  layout->addWidget(fontLabel,  0, 1);
  fontDlgOptions = new DialogOptionsWidget;
  fontDlgOptions->addCheckBox("Do not use native dialog", QFontDialog::DontUseNativeDialog);
  fontDlgOptions->addCheckBox("Show scalable fonts", QFontDialog::ScalableFonts);
  fontDlgOptions->addCheckBox("Show monospaced fonts", QFontDialog::MonospacedFonts);
  fontDlgOptions->addCheckBox("Show proportional fonts", QFontDialog::ProportionalFonts);
  fontDlgOptions->addCheckBox("No buttons", QFontDialog::NoButtons);
  layout->addItem(new QSpacerItem(0, 0, QSizePolicy::Ignored, QSizePolicy::MinimumExpanding), 1, 0);
  layout->addWidget(fontDlgOptions, 2, 0, 1, 2);
  toolbox->addItem(page, "Font Dialog");

  // file dialog page
  page = new QWidget;
  layout = new QGridLayout(page);

  directoryLabel = new QLabel;
  directoryLabel->setFrameStyle(frameStyle);
  auto directoryButton = new QPushButton("QFileDialog::getExistingDirectory()");

  openFileNameLabel = new QLabel;
  openFileNameLabel->setFrameStyle(frameStyle);
  auto openFileButton = new QPushButton("QFileDialog::getOpenFileName()");

  openFileNamesLabel = new QLabel;
  openFileNamesLabel->setFrameStyle(frameStyle);
  auto openFileNamesButton = new QPushButton("QFileDialog::getOpenFileNames()");

  saveFileNameLabel = new QLabel;
  saveFileNameLabel->setFrameStyle(frameStyle);
  auto saveFileNameButton = new QPushButton("QFileDialog::getSaveFileName()");

  connect(directoryButton, &QAbstractButton::clicked, this, &Dialog::setExistingDirectory);
  connect(openFileButton, &QAbstractButton::clicked, this, &Dialog::setOpenFileName);
  connect(openFileNamesButton, &QAbstractButton::clicked, this, &Dialog::setOpenFileNames);
  connect(saveFileNameButton, &QAbstractButton::clicked, this, &Dialog::setSaveFileName);

  layout->setColumnStretch(1, 1);
  layout->addWidget(directoryButton, 0, 0);
  layout->addWidget(directoryLabel, 0, 1);
  layout->addWidget(openFileButton, 1, 0);
  layout->addWidget(openFileNameLabel, 1, 1);
  layout->addWidget(openFileNamesButton, 2, 0);
  layout->addWidget(openFileNamesLabel, 2, 1);
  layout->addWidget(saveFileNameButton, 3, 0);
  layout->addWidget(saveFileNameLabel, 3, 1);

  fileDlgOptions = new DialogOptionsWidget;
  fileDlgOptions->addCheckBox("Do not use native dialog", QFileDialog::DontUseNativeDialog);
  fileDlgOptions->addCheckBox(("Show directories only"), QFileDialog::ShowDirsOnly);
  fileDlgOptions->addCheckBox(("Do not resolve symlinks"), QFileDialog::DontResolveSymlinks);
  fileDlgOptions->addCheckBox(("Do not confirm overwrite"), QFileDialog::DontConfirmOverwrite);
  fileDlgOptions->addCheckBox(("Do not use sheet"), QFileDialog::DontUseSheet);
  fileDlgOptions->addCheckBox(("Readonly"), QFileDialog::ReadOnly);
  fileDlgOptions->addCheckBox(("Hide name filter details"), QFileDialog::HideNameFilterDetails);
  fileDlgOptions->addCheckBox(("Do not use custom directory icons (Windows)"), QFileDialog::DontUseCustomDirectoryIcons);
  layout->addItem(new QSpacerItem(0, 0, QSizePolicy::Ignored, QSizePolicy::MinimumExpanding), 4, 0);
  layout->addWidget(fileDlgOptions, 5, 0, 1 ,2);
  toolbox->addItem(page, tr("File Dialogs"));
  
  // messageBoxes
  page = new QWidget;
  layout = new QGridLayout(page);

  criticalLabel = new QLabel;
  criticalLabel->setFrameStyle(frameStyle);
  auto criticalButton = new QPushButton("QMessageBox::critical()");

  informationLabel = new QLabel;
  informationLabel->setFrameStyle(frameStyle);
  auto informationButton = new QPushButton("QMessageBox::information()");

  questionLabel = new QLabel;
  questionLabel->setFrameStyle(frameStyle);
  auto questionButton = new QPushButton("QMessageBox::question");

  warningLabel = new QLabel;
  warningLabel->setFrameStyle(frameStyle);
  auto warningButton = new QPushButton(tr("QMessageBox::&warning()"));

  errorLabel = new QLabel;
  errorLabel->setFrameStyle(frameStyle);
  auto errorButton = new QPushButton(tr("QErrorMessage::showM&essage()"));

  connect(criticalButton, &QAbstractButton::clicked, this, &Dialog::criticalMessage);
  connect(informationButton, &QAbstractButton::clicked, this, &Dialog::informationMessage);
  connect(questionButton, &QAbstractButton::clicked, this, &Dialog::questionMessage);
  connect(warningButton, &QAbstractButton::clicked, this, &Dialog::warningMessage);
  connect(errorButton, &QAbstractButton::clicked, this, &Dialog::errorMessage);

  layout->setColumnStretch(1, 1);
  layout->addWidget(criticalButton, 0, 0);
  layout->addWidget(criticalLabel,  0, 1);
  layout->addWidget(informationButton, 1, 0);
  layout->addWidget(informationLabel,  1, 1);
  layout->addWidget(questionButton, 2, 0);
  layout->addWidget(questionLabel,  2, 1);
  layout->addWidget(warningButton, 3, 0);
  layout->addWidget(warningLabel,  3, 1);
  layout->addWidget(errorButton, 4, 0);
  layout->addWidget(errorLabel,  4, 1);
  layout->addItem(new QSpacerItem(0, 0, QSizePolicy::Ignored, QSizePolicy::MinimumExpanding), 2, 0);
  toolbox->addItem(page, "Message Dialog");

  this->setWindowTitle(QGuiApplication::applicationDisplayName());
}

Dialog::~Dialog()
{
}

void Dialog::setInteger()
{ 
  bool ok;
  auto result = QInputDialog::getInt(this, 
      "QInputDialog::getInteger()",
       tr("Percentage:"), 25, 0, 100, 1, &ok);

  if (ok)
    integerLabel->setText(tr("%1%").arg(result));
}

void Dialog::setDouble()
{
  auto ok = false;
  auto result = QInputDialog::getDouble(this, 
      "QInputDialog::getDouble()",
      tr("Percentage:"), 37.56, -1000, 1000, 2, &ok);

  if (ok)
    doubleLabel->setText(tr("$%1").arg(result));
}

void Dialog::setItem()
{
  QStringList items;
  items << tr("Spring") << tr("Summer") << tr("Fall") << tr("Winter");

  bool ok;
  auto item = QInputDialog::getItem(this, tr("QInputDialog::getItem()"),
      tr("Season:"), items, 0, false, &ok);

  if (ok && !item.isEmpty())
    itemLabel->setText(item);
}

void Dialog::setText()
{
  bool ok;
  auto text = QInputDialog::getText(this, tr("QInputDialog::getText()"),
      tr("User Name:"), QLineEdit::Normal, QDir::home().dirName(), &ok);

  if (ok && !text.isEmpty())
    textLabel->setText(text);
}

void Dialog::setMultilineText()
{
  bool ok;
  auto text = QInputDialog::getMultiLineText(this, tr("QInputDialog::getMultiLineText()"),
      tr("소개:"), "우리나라 좋은나라\n대한민국", &ok);

  if (ok && !text.isEmpty())
    mlTextLabel->setText(text);
}

void Dialog::setColor()
{
  auto options = QFlag(colorDlgOptions->value());
  auto color   = QColorDialog::getColor(Qt::green, this, "Select Color", options);

  if (color.isValid())
  {
    colorLabel->setText(color.name());
    colorLabel->setPalette(QPalette(color));
    colorLabel->setAutoFillBackground(true);
  }
}

void Dialog::setFont()
{
  auto options = QFlag(fontDlgOptions->value());
  bool ok;
  auto font = QFontDialog::getFont(&ok, QFont(fontLabel->text()), this, "Select Font", options);
  if (ok)
  {
    fontLabel->setText(font.key());
    fontLabel->setFont(font);
  }
}

void Dialog::setOpenFileName()
{
  auto options = QFlag(fileDlgOptions->value());
  QString selectedFilter;
  auto fileName = QFileDialog::getOpenFileName(this, 
      "QFileDialog::getOpenFileName()",
      openFileNameLabel->text(),
      "All Files (*);; Text Files (*.txt)",
      &selectedFilter,
      options);

  if (!fileName.isEmpty())
    openFileNameLabel->setText(fileName);
}

void Dialog::setOpenFileNames()
{
  auto options = QFlag(fileDlgOptions->value());
  QString selectedFilter;
  auto files = QFileDialog::getOpenFileNames(this,
      "QFileDialog::getOpenFileNames()",
      openFilePath,
      "All Files (*);;Text Files (*.txt)",
      &selectedFilter,
      options);

  if (files.count() != 0)
  {
    openFilePath = files[0];
    openFileNamesLabel->setText(QString("[%1").arg(files.join(", ")));
  }
}

void Dialog::setExistingDirectory()
{
  QFileDialog::Options options = QFlag(fileDlgOptions->value());
  options |= QFileDialog::DontResolveSymlinks | QFileDialog::ShowDirsOnly;

  auto directory = QFileDialog::getExistingDirectory(this,
      "QFileDialog::getExistingDirectory()",
      directoryLabel->text(),
      options);

  if (!directory.isEmpty())
    directoryLabel->setText(directory);
}

void Dialog::setSaveFileName()
{
  auto options = QFlag(fileDlgOptions->value());
  QString selectedFilter;
  auto fileName = QFileDialog::getSaveFileName(this,
      "QFileDialog::getSaveFileName()",
      saveFileNameLabel->text(),
      "All Files(*);;Text Files (*.txt)",
      &selectedFilter,
      options);

  if (!fileName.isEmpty())
    saveFileNameLabel->setText(fileName);
}

void Dialog::criticalMessage()
{
  auto reply = QMessageBox::critical(this, "QMessageBox::critical()",
      MESSAGE,
      QMessageBox::Abort | QMessageBox::Retry | QMessageBox::Ignore);

  if (reply == QMessageBox::Abort) criticalLabel->setText("Abort");
  if (reply == QMessageBox::Retry) criticalLabel->setText("Retry");
  if (reply == QMessageBox::Ignore) criticalLabel->setText("Ignore");
}

void Dialog::informationMessage()
{
  auto reply = QMessageBox::information(this, "QMessageBox::information()", MESSAGE);

  auto msg = reply == QMessageBox::Ok ? "OK" : "Escape";
  informationLabel->setText(msg);
}

void Dialog::questionMessage()
{
  auto reply = QMessageBox::question(this, "QMessageBox::question()", 
      MESSAGE,
      QMessageBox::Yes | QMessageBox::No | QMessageBox::Cancel);

  if (reply == QMessageBox::Yes)
    questionLabel->setText("Yes");

  if (reply == QMessageBox::No)
    questionLabel->setText("No");

  if (reply == QMessageBox::Cancel)
    questionLabel->setText("Cancel");
}

void Dialog::warningMessage()
{
  QMessageBox msgBox(QMessageBox::Warning, tr("QMessageBox::warning()"), MESSAGE, nullptr, this);
  msgBox.setDetailedText(MESSAGE_DETAILS);
  msgBox.addButton(tr("Save &Again"), QMessageBox::AcceptRole);
  msgBox.addButton(tr("&Continue"), QMessageBox::RejectRole);

  if (msgBox.exec() == QMessageBox::AcceptRole)
    warningLabel->setText(tr("Save Again"));
  else
    warningLabel->setText(tr("Continue"));
}

void Dialog::errorMessage()
{
  errorMessageDlg->showMessage(
      tr("This dialog shows and remembers error messages. "
        "If the checkbox is checked (as it is by default), "
        "the shown message will be shown again, "
        "but if the user unchecks the box the message "
        "will not appear again if QErrorMessage::showMessage() "
        "is called with the same message."));
  errorLabel->setText(tr("If the box is unchecked, the message "
        "won't appear again."));
}
