#ifndef VIEW_H
#define VIEW_H

#include <QtWidgets/QGraphicsView>
#include <QtCharts/QChartGlobal>

QT_BEGIN_NAMESPACE
class QGraphicsScene;
class QMouseEvent;
class QResizeEvent;
QT_END_NAMESPACE

QT_CHARTS_BEGIN_NAMESPACE
class QChart;
QT_CHARTS_END_NAMESPACE

class Callout;

QT_CHARTS_USE_NAMESPACE

class View : public QGraphicsView
{
  Q_OBJECT

public:
  View(QWidget *parent = 0);

public slots:
  void keepCallout();
  void tooltip(QPointF point, bool state);

protected:
  void resizeEvent(QResizeEvent* event);
  void mouseMoveEvent(QMouseEvent* event);

private:
  QGraphicsSimpleTextItem* coordX;
  QGraphicsSimpleTextItem* coordY;
  QChart* chart;
  Callout* tooltip_;
  QList<Callout *> callouts;
};

#endif // VIEW_H
