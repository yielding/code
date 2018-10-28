#include "mainwindow.h"
#include "renderarea.h"

#include <QtWidgets>

MainWindow::MainWindow(QWidget *parent)
    : QWidget(parent)
{
  renderArea = new RenderArea(this);

  penWidthSpinBox = new QSpinBox;
  penWidthSpinBox->setRange(0, 20);
  penWidthSpinBox->setSpecialValueText(tr("0 (cosmetic pen)"));

  penWidthLabel = new QLabel(tr("펜 굵기"));
  penWidthLabel->setBuddy(penWidthSpinBox);

  resetButton = new QPushButton(tr("&Reset"));
  darwButton  = new QPushButton(tr("&Draw"));

  // connect
  connect(penWidthSpinBox, SIGNAL(valueChanged(int)), this, SLOT(penChanged()));
  connect(resetButton,     SIGNAL(clicked()), this, SLOT(resetClicked()));
  connect(darwButton,      SIGNAL(clicked()), this, SLOT(drawClicked()));

  // layout
  auto mainLayout = new QGridLayout;

  mainLayout->addWidget(renderArea, 0, 0, 1, 4);
  mainLayout->addWidget(penWidthLabel, 2, 0, Qt::AlignRight);
  mainLayout->addWidget(penWidthSpinBox, 2, 1);
  mainLayout->addWidget(darwButton,  2, 2);
  mainLayout->addWidget(resetButton, 2, 3);

  setLayout(mainLayout);
  setWindowTitle(tr("Convex Hull"));
}

MainWindow::~MainWindow()
{
}

void MainWindow::penChanged()
{
  auto width = 2;
  auto style = Qt::PenStyle(1);
  auto cap   = Qt::PenCapStyle(1);
  auto join  = Qt::PenJoinStyle(0);

  renderArea->setPen(QPen(Qt::blue, width, style, cap, join));
}

void MainWindow::brushChanged()
{
  auto style = Qt::BrushStyle(0);

  renderArea->setBrush(QBrush(Qt::green, style));
}

void MainWindow::resetClicked()
{
  renderArea->clearArea();
}

void MainWindow::drawClicked()
{
  renderArea->findHull();
}
