#include "dialog.h"

#include <QtWidgets>

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

  // file dialog page
  
  // messageBoxes
  
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
  bool ok;
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

