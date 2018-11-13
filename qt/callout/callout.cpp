#include "callout.h"

#include <QtGui/QPainter>
#include <QtGui/QFontMetrics>
#include <QtGui/QMouseEvent>
#include <QtWidgets/QGraphicsSceneMouseEvent>
#include <QtCharts/QChart>

Callout::Callout(QChart *crt) 
  : QGraphicsItem(crt)
  , chart(crt)
{
}

auto Callout::boundingRect() const -> QRectF
{
  auto anchor = mapFromParent(chart->mapToPosition(this->anchor));
  QRectF r;

  r.setLeft  (qMin(rect.left(),   anchor.x()));
  r.setRight (qMax(rect.right(),  anchor.x()));
  r.setTop   (qMin(rect.top(),    anchor.y()));
  r.setBottom(qMax(rect.bottom(), anchor.y()));

  return rect;
}

void Callout::paint(QPainter* painter, const QStyleOptionGraphicsItem* option, QWidget* w)
{
  Q_UNUSED(option);
  Q_UNUSED(w);

  QPainterPath path;
  path.addRoundedRect(rect, 5, 5);

  auto anchor = mapFromParent(chart->mapToPosition(this->anchor));
  if (!rect.contains(anchor)) 
  {
    QPointF point1, point2;

    // establish the position of the anchor point in relation to rect
    auto above = anchor.y() <= rect.top();
    auto aboveCenter = anchor.y() > rect.top() && anchor.y() <= rect.center().y();
    auto belowCenter = anchor.y() > rect.center().y() && anchor.y() <= rect.bottom();
    auto below = anchor.y() > rect.bottom();

    auto onLeft = anchor.x() <= rect.left();
    auto leftOfCenter = anchor.x() > rect.left() && anchor.x() <= rect.center().x();
    auto rightOfCenter = anchor.x() > rect.center().x() && anchor.x() <= rect.right();
    auto onRight = anchor.x() > rect.right();

    // get the nearest rect corner.
    auto x = (onRight + rightOfCenter) * rect.width();
    auto y = (below + belowCenter) * rect.height();
    auto cornerCase = (above && onLeft) || (above && onRight) || (below && onLeft) || (below && onRight);
    auto vertical   = qAbs(anchor.x() - x) > qAbs(anchor.y() - y);

    auto x1 = x + leftOfCenter * 10 - rightOfCenter * 20 + cornerCase * !vertical * (onLeft * 10 - onRight * 20);
    auto y1 = y + aboveCenter * 10 - belowCenter * 20 + cornerCase * vertical * (above * 10 - below * 20);;
    point1.setX(x1);
    point1.setY(y1);

    auto x2 = x + leftOfCenter * 20 - rightOfCenter * 10 + cornerCase * !vertical * (onLeft * 20 - onRight * 10);;
    auto y2 = y + aboveCenter * 20 - belowCenter * 10 + cornerCase * vertical * (above * 20 - below * 10);;
    point2.setX(x2);
    point2.setY(y2);

    path.moveTo(point1);
    path.lineTo(anchor);
    path.lineTo(point2);
    path = path.simplified();
  }

  painter->setBrush(QColor(255, 255, 255));
  painter->drawPath(path);
  painter->drawText(textRect, text);
}

void Callout::mousePressEvent(QGraphicsSceneMouseEvent* event)
{
  event->setAccepted(true);
}

void Callout::mouseMoveEvent(QGraphicsSceneMouseEvent* event)
{
  if (event->buttons() & Qt::LeftButton)
  {
    setPos(mapToParent(event->pos() - event->buttonDownPos(Qt::LeftButton)));
    event->setAccepted(true);
  }
  else
  {
    event->setAccepted(false);
  }
}

void Callout::setText(const QString& text)
{
  this->text = text;
  QFontMetrics metrics(font);
  textRect = metrics.boundingRect(QRect(0, 0, 150, 150), Qt::AlignLeft, text);
  textRect.translate(5, 5);
  prepareGeometryChange();
  rect = textRect.adjusted(-5, -5, 5, 5);
}

void Callout::setAnchor(QPointF point)
{
  anchor = point;
}

void Callout::updateGeometry()
{
  prepareGeometryChange();
  setPos(chart->mapToPosition(anchor) + QPoint(10, -50));
}
