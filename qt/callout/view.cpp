#include "view.h"
#include "callout.h"

#include <QtGui/QResizeEvent>
#include <QtGui/QMouseEvent>
#include <QtWidgets/QGraphicsScene>
#include <QtCharts/QChart>
#include <QtCharts/QLineSeries>
#include <QtCharts/QSplineSeries>
#include <QtWidgets/QGraphicsTextItem>

View::View(QWidget *parent)
  : QGraphicsView(new QGraphicsScene, parent)
  , coordX(nullptr)
  , coordY(nullptr)
  , chart(nullptr)
  , tooltip_(nullptr)
{
  setDragMode(QGraphicsView::NoDrag);
  setVerticalScrollBarPolicy(Qt::ScrollBarAlwaysOff);
  setHorizontalScrollBarPolicy(Qt::ScrollBarAlwaysOff);

  chart = new QChart;
  chart->setMinimumSize(640, 480);
  chart->setTitle("이 라인위를 마우스가 지나면 callout이 나타난다");
  chart->legend()->hide();

  auto series = new QLineSeries;
  series->append(1, 3);
  series->append(4, 5);
  series->append(5, 4.5);
  series->append(7, 1);
  series->append(11, 2);
  chart->addSeries(series);

  auto series2 = new QSplineSeries;
  series2->append(1.6, 1.4);
  series2->append(2.4, 3.5);
  series2->append(3.7, 2.5);
  series2->append(7, 4);
  series2->append(10, 2);
  chart->addSeries(series2);

  chart->createDefaultAxes();
  chart->setAcceptHoverEvents(true);
  setRenderHint(QPainter::Antialiasing);
  scene()->addItem(chart);

  coordX = new QGraphicsSimpleTextItem(chart);
  coordX->setPos(chart->size().width()/2 - 50, chart->size().height());
  coordX->setText("X: ");

  coordY = new QGraphicsSimpleTextItem(chart);
  coordY->setPos(chart->size().width()/2 + 50, chart->size().height());
  coordY->setText("Y: ");

  connect(series, &QLineSeries::clicked, this, &View::keepCallout);
  connect(series, &QLineSeries::hovered, this, &View::tooltip);

  connect(series2, &QLineSeries::clicked, this, &View::keepCallout);
  connect(series2, &QLineSeries::hovered, this, &View::tooltip);

  setMouseTracking(true);
}

void View::resizeEvent(QResizeEvent* event)
{
  if (scene()) 
  {
    scene()->setSceneRect(QRect(QPoint(0, 0), event->size()));
    chart->resize(event->size());
    coordX->setPos(chart->size().width()/2 - 50, chart->size().height() - 20);
    coordY->setPos(chart->size().width()/2 + 50, chart->size().height() - 20);
    const auto couts = callouts;
    for (Callout *callout : couts)
      callout->updateGeometry();
  }

  QGraphicsView::resizeEvent(event);
}

void View::mouseMoveEvent(QMouseEvent* event)
{
  coordX->setText(QString("X: %1").arg(chart->mapToValue(event->pos()).x()));
  coordY->setText(QString("Y: %1").arg(chart->mapToValue(event->pos()).y()));
  QGraphicsView::mouseMoveEvent(event);
}


void View::keepCallout()
{
  callouts.append(tooltip_);
  tooltip_ = new Callout(chart);
}

void View::tooltip(QPointF point, bool state)
{
  if (tooltip_ == nullptr)
    tooltip_ = new Callout(chart);

  if (state) 
  {
    tooltip_->setText(QString("X: %1 \nY: %2 ").arg(point.x()).arg(point.y()));
    tooltip_->setAnchor(point);
    tooltip_->setZValue(11);
    tooltip_->updateGeometry();
    tooltip_->show();
  } 
  else 
  {
    tooltip_->hide();
  }
}
