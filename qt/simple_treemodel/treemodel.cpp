
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
  if (!index.isValid())
    return QVariant();

  if (role != Qt::DisplayRole)
    return QVariant();

  auto item = static_cast<TreeItem*>(index.internalPointer());
  return item->data(index.column());
}

auto TreeModel::flags(const QModelIndex& index) const -> Qt::ItemFlags
{
  if (!index.isValid())
    return Qt::ItemFlag::NoItemFlags;

  return QAbstractItemModel::flags(index);
}

auto TreeModel::headerData(int section, Qt::Orientation orientation, int role) const -> QVariant
{
  if (orientation == Qt::Horizontal && role == Qt::DisplayRole)
    return rootItem->data(section);

  return QVariant();
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

void TreeModel::setupModelData(const QStringList& lines, TreeItem* parent)
{
  QList<TreeItem*> parents;
  QList<int> indentations;

  parents << parent;
  indentations << 0;

  int number = 0;
  while (number < lines.count())
  {
    int position = 0;
    while (position < lines[number].length())
    {
      if (lines[number].at(position) != ' ') break;
      position++;
    }

    auto lineData = lines[number].mid(position).trimmed();
    if (!lineData.isEmpty())
    {
      auto columnStrings = lineData.split("\t", QString::KeepEmptyParts);
      QList<QVariant> columnData;
      for (int column = 0; column < columnStrings.count(); ++column) 
        columnData << columnStrings[column];

      if (position > indentations.last())
      {
        if (parents.last()->childCount() > 0) 
        {
          parents << parents.last()->child(parents.last()->childCount() - 1); indentations << position;
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

    ++number;
  }
}
