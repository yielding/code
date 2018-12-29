#ifndef TREEITEM_H
#define TREEITEM_H

#include <QList>
#include <QVariant>

class TreeItem
{
public:
  explicit TreeItem(const QList<QVariant>& data, TreeItem* parent = nullptr);
  ~TreeItem();

public:
  auto appendChild(TreeItem* child) -> void;
  auto child(int row) -> TreeItem*;
  auto childCount()  const -> int;
  auto columnCount() const -> int;
  auto data(int column) const -> QVariant;
  auto row() const -> int;
  auto parentItem() -> TreeItem*;

private:
  QList<TreeItem*> m_childItems;
  QList<QVariant> m_itemData;
  TreeItem* m_parentItem;
};

#endif // TREEITEM_H
