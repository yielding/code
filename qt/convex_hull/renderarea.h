#ifndef RENDERAREA_H
#define RENDERAREA_H

#include "color_point.h"
#include "jarvis_march.h"

#include <QWidget>
#include <QPen>
#include <QBrush>

#include <vector>
class QPen;
class QBrush;
class QPainterPath;
class QAction;

// TODO
// find graceful way
const int SZ = 7;

typedef ColorPoint<QColor> Point;

class RenderArea: public QWidget
{
  Q_OBJECT

public:
  explicit RenderArea(QWidget *parent = nullptr);

  auto minimumSizeHint() const -> QSize override;
  auto sizeHint() const -> QSize override;
  auto numberOfPoint(int count) -> void;

public slots:
  void setPen(const QPen& pen);
  void setBrush(const QBrush& brush);
  void clearArea();
  void findHull();

private slots:
  void selectBgColor(const QPoint&);
  void changeBgColor(QColor color);

protected:
  auto paintEvent(QPaintEvent* event) -> void override;
  auto mousePressEvent(QMouseEvent* event) -> void override;
  auto mouseReleaseEvent(QMouseEvent* event) -> void override;
  auto mouseMoveEvent(QMouseEvent* event) -> void override;
  // auto contextMenuEvent(QContextMenuEvent* event) -> void override;

private:
  auto randomColor() -> QColor;
  auto randomPoint() -> ColorPoint<QColor>;
  void drawPoint(QPainter& paitnter, const Point& point);
  void drawHull(QPainter& paitnter);

private:
  QPen pen;
  QBrush brush;

  std::vector<Point> points;
  std::vector<Point>::iterator selected;
  JarvisMarch<QColor> hull;
  bool poly_mode = false;
  int  count {1};
};

#endif // RENDERAREA_H
