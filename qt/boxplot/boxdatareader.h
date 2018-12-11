#ifndef BOXDATAREADER_H
#define BOXDATAREADER_H

#include <QtCore/QTextStream>
#include <QtCharts/QBoxSet>

QT_CHARTS_USE_NAMESPACE

class BoxDataReader: public QTextStream 
{
public:
  BoxDataReader(QIODevice*);

  auto readBox() -> QBoxSet*;
  auto readFile(QIODevice* file) -> void;

protected:
  auto findMedian(int begin, int end) -> qreal;

private:
  QList<qreal> sortedList;
};

#endif 
