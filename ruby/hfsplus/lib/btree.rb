require "structs"

class BTree
  def initialize(file, key_class, data_class)
     @file = file
     @key_class = key_class
     @data_class = data_class
     @block0 = @file.read_block_at 0
     btnode  = BTNodeDescriptor.read(@block0)

     p btnode
     raise "Wrong btree header" unless btnode.kind == KBTHeaderNode
  end

  def node_in_use? node_no
  end

  def read_empty_space
  end

  def compare_keys(k1, k2)
  end

  def print_leaf(key, data)
  end

  def read_btree_node(node_no)
  end

  def search(key, node=nil)
  end

  def traverse(node=nil, count=0, callback=nil)
  end

  def traverse_leaf_nodes(callback=nil)
  end

  def search_multiple(search_key)
  end
end

class CatalogTree < BTree
  def initialize file
    super(file, HFSPlusCatalogKey, HFSPlusCatalogData)
  end

end

class ExtentsOverflowTree < BTree
  def initialize file
    super(file, HFSPlusExtentKey, HFSPlusExtentRecord)
  end

end

class AttributesTree < BTree
  def initialize file
    super(file, HFSPlusAttrKey, HFSPlusAttrData)

  end
end
