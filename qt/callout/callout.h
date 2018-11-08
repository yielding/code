#ifndef CALLOUT_H
#define CALLOUT_H

#include <QtCharts/QChartGlobal>
#include <QtWidgets/QGraphicsItem>
#include <QtGui/QFont>

QT_CHARTS_BEGIN_NAMESPACE
class QChart;
QT_CHARTS_END_NAMESPACE

QT_CHARTS_USE_NAMESPACE

class Callout : public QGraphicsItem
{
public:
  explicit Callout(QChart *parent = nullptr);

public:
  void setText(const QString& text);
  void setAnchor(QPointF point);
  void updateGeometry();

  auto boundingRect() const -> QRectF;
  void paint(QPainter* painter, const QStyleOptionGraphicsItem* opt, QWidget* w);

protected:
  void mousePressEvent(QGraphicsSceneMouseEvent* event);
  void mouseMoveEvent(QGraphicsSceneMouseEvent* event);

private:
  QString text;
  QRectF textRect;
  QRectF rect;
  QPointF anchor;
  QFont font;
  QChart* chart;
};

#endif // CALLOUT_H
