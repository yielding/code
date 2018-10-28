#ifndef MAINWINDOW_H
#define MAINWINDOW_H

#include <QWidget>

class QSpinBox;
class QLabel;
class QPushButton;

class RenderArea;

class MainWindow : public QWidget
{
  Q_OBJECT

public:
  MainWindow(QWidget *parent = nullptr);
  ~MainWindow();

private slots:
  void penChanged();
  void brushChanged();
  void resetClicked();
  void drawClicked();

private:
  RenderArea* renderArea;

  QLabel* penWidthLabel;
  QSpinBox* penWidthSpinBox;
  QPushButton* resetButton;
  QPushButton* darwButton;
};

#endif // MAINWINDOW_H
