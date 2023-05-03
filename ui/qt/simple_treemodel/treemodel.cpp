#include "treeitem.h"
#include "treemodel.h"

#include <QStringList>

TreeModel::TreeModel(const QString& data, QObject* parent)
  : QAbstractItemModel(parent)
{
  QList<QVariant> rootData;
  rootData << "Title" << "Summary";
  rootItem = new TreeItem(rootData);
  setupModelData(data.split(QString("\n")), rootItem);
}

TreeModel::~TreeModel()
{
  delete rootItem;
}

auto TreeModel::columnCount(const QModelIndex& parent) const -> int
{
  return parent.isValid()
    ? static_cast<TreeItem*>(parent.internalPointer())->columnCount()
    : rootItem->columnCount();
}

auto TreeModel::data(const QModelIndex& index, int role) const -> QVariant
{
  if (!index.isValid() || role != Qt::DisplayRole)
    return QVariant();

  auto item = static_cast<TreeItem*>(index.internalPointer());

  return item->data(index.column());
}

auto TreeModel::flags(const QModelIndex& index) const -> Qt::ItemFlags
{
  return index.isValid()
    ? QAbstractItemModel::flags(index)
    : Qt::ItemFlag::NoItemFlags;
}

auto TreeModel::headerData(int section, Qt::Orientation orientation, int role) const -> QVariant
{
  return (orientation == Qt::Horizontal && role == Qt::DisplayRole)
    ? rootItem->data(section)
    : QVariant();
}

auto TreeModel::index(int row, int column, const QModelIndex& parent ) const -> QModelIndex
{
  if (this->hasIndex(row, column, parent))
  {
    auto parentItem = !parent.isValid()
      ? rootItem
      : static_cast<TreeItem*>(parent.internalPointer());

    auto childItem = parentItem->child(row);
    if (childItem)
      return createIndex(row, column, childItem);
  }

  return QModelIndex();
}

auto TreeModel::parent(const QModelIndex& index) const -> QModelIndex
{
  if (!index.isValid())
    return QModelIndex();

  auto childItem  = static_cast<TreeItem*>(index.internalPointer());
  auto parentItem = childItem->parentItem();

  return (parentItem == rootItem)
    ? QModelIndex()
    : createIndex(parentItem->row(), 0, parentItem);
}

auto TreeModel::rowCount(const QModelIndex& parent) const -> int
{
  if (parent.column() > 0)
    return 0;

  auto parentItem = !parent.isValid()
    ? rootItem
    : static_cast<TreeItem*>(parent.internalPointer());

  return parentItem->childCount();
}

auto TreeModel::setupModelData(const QStringList& lines, TreeItem* parent) -> void
{
  QList<TreeItem*> parents { parent };
  QList<int> indentations { 0 };

  for (auto const& line: lines)
  {
    int position = 0;
    for (; position < line.length(); ++position)
      if (line.at(position) != ' ') break;

    auto lineData = line.mid(position).trimmed();
    if (!lineData.isEmpty())
    {
      auto columnStrings = lineData.split("\t", QString::KeepEmptyParts);
      QList<QVariant> columnData;
      for (auto col: columnStrings) columnData << col;

      if (position > indentations.last())
      {
        if (parents.last()->childCount() > 0) 
        {
          parents << parents.last()->child(parents.last()->childCount() - 1); 
          indentations << position;
        }
      }
      else
      {
        while (position < indentations.last() && parents.count() > 0)
        {
          parents.pop_back();
          indentations.pop_back();
        }
      }

      parents.last()->appendChild(new TreeItem(columnData, parents.last()));
    }
  }
}
