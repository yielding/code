#include "treeitem.h"

#include <QStringList>

TreeItem::TreeItem(const QList<QVariant>& data, TreeItem* parent)
{
  m_parentItem = parent;
  m_itemData = data;
}

TreeItem::~TreeItem()
{
  qDeleteAll(m_childItems);
}

auto TreeItem::appendChild(TreeItem* item) -> void
{
  m_childItems.append(item);
}

auto TreeItem::child(int row) -> TreeItem*
{
  return m_childItems.value(row);
}

auto TreeItem::childCount() const -> int
{
  return m_childItems.count();
}

auto TreeItem::columnCount() const -> int
{
  return m_itemData.count();
}

auto TreeItem::data(int column) const -> QVariant
{
  return m_itemData.value(column);
}

auto TreeItem::parentItem() -> TreeItem*
{
  return m_parentItem;
}

auto TreeItem::row() const -> int
{
  return m_parentItem
    ? m_parentItem->m_childItems.indexOf(const_cast<TreeItem*>(this))
    : 0;
}
