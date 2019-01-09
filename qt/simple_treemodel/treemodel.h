#ifndef TREEMODEL_H_
#define TREEMODEL_H_

#include <QAbstractItemModel>
#include <QModelIndex>
#include <QVariant>

class TreeItem;

class TreeModel: public QAbstractItemModel
{
  Q_OBJECT

public:
  explicit TreeModel(const QString& data, QObject* parent = nullptr);
  ~TreeModel() override;

public:
  auto data (const QModelIndex& index, int role) const -> QVariant override;
  auto flags(const QModelIndex& index) const -> Qt::ItemFlags override;
  auto headerData(int section, Qt::Orientation Orientation,
      int role = Qt::DisplayRole) const -> QVariant override;

  auto index(int row, int col, const QModelIndex& parent = QModelIndex()) const -> QModelIndex override;
  auto parent(const QModelIndex& index) const -> QModelIndex override;
  auto rowCount(const QModelIndex& parent = QModelIndex())    const -> int override;
  auto columnCount(const QModelIndex& parent = QModelIndex()) const -> int override;

private:
  auto setupModelData(const QStringList& lines, TreeItem* parent) -> void;

private:
  TreeItem* rootItem;
};

#endif 
