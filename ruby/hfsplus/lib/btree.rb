require "structs"

class BTree
  def initialize(file, key_class, data_class)
     @file = file
     @key_class = key_class
     @data_class = data_class
     block0 = @file.read_block_at 0
     btnode = BTNodeDescriptor.read block0 

     p btnode
     raise "Wrong btree header" unless btnode.kind == KBTHeaderNode

     sz = btnode.num_bytes
     @header    = BTHeaderRec.read(block0[sz, block0.size - sz])
     @node_size = @header.nodeSize
     @nodes_in_block  = @file.block_size / @header.nodeSize
     @blocks_per_node = @header.nodeSize / @file.block_size
     @last_record_no  = 0
     # TODO here
     type, (hdr, maprec) = self.read_btree_node 0
     @maprec = maprec
  end

  def node_in_use? node_no
    byte = @maprec[node_no / 8].ord

    return (byte & (1 << (7 - (node_no % 8)))) != 0
  end

  def read_empty_space
  end

  def compare_keys(k1, k2)
  end

  def print_leaf(key, data)
  end

  def read_node nth
    node = ""
    0.upto @blocks_per_node-1 do |i|
      index = nth * @blocks_per_node + i
      node += @file.read_block_at index
    end

    node
  end

  # REMARK: why backup the node?
  def read_btree_node nth
    @last_node_no = nth
    node = read_node(nth)
    @last_btnode = btnode = BTNodeDescriptor.read(node)

    arr = BinData::Array.new(:type => :uint16be, :initial_length => btnode.numRecords)
    offsets = arr.read(node[-2*btnode.numRecords..-1])

    if btnode.kind == KBTHeaderNode
      header = BTHeaderRec.read(node[14..-1])
      maprec = node[offsets[-3]..-1]
      return KBTHeaderNode, [header, maprec]
    elsif btnode.kind == KBTIndexNode
#       recs = []
#       for i in (0...btnode.numRecords)
#         offset = offsets[btnode.numRecords-i-1]
#         key    = @key_class.read(node[offset..-1])
#         offset += 2 + key.keyLength
#         key.childNode
#       end
    elsif btnode.kind == KBTLeafNode
      recs = []
      for i in (0...btnode.numRecords)
        offset = offsets[btnode.numRecords - i - 1]
        key    = @key_class.read(node[offset..-1])
        offset += 2 + key.keyLength
        data   = @data_class.read(node[offset..-1])
        #
        # [key, data] 이걸 루비 tuple로 어떻게 표현?
        # {key, value}
        #
        recs << [key, data]
      end

      return KBTLeafNode, recs
    else
      raise Excention, "read_btree.node error"
    end
  end

  def search(key, node=nil)
    node = @header.rootNode if node.nil?
    type, stuff = read_btree_node(node)

    if type == KBTIndexNode
      #
      #
    elsif type == KBTLeafNode
      #
      #
    end

    return nil, nil
  end

  def traverse(node=nil, count=0, callback=nil)
    node = @header.rootNode if node.nil?
    type, stuff = read_btree_node(node)

    if type == KBTIndexNode
      #
      #
    elsif type == KBTLeafNode
      #
      #
    end

    return nil, nil
  end

  def traverse_leaf_nodes(callback=nil)
    node_no = @header.firstLeafNode
    count   = 0
    while node_no != 0
      _, stuff = read_btree_node(node_no)
      count   += stuff.size

      for k, v in stuff
        unless callback.nil?
          callback(k, v)
        else
          print_leaf(k, v)
        end
      end

      node_no = @last_btnode.fLink
    end

    return count
  end

  #
  # TODO
  #
  def search_multiple(search_key, filter_key_function=nil)
    search(key)
    node_no   = @last_node_no
    record_no = @last_record_no
    kv        = []
    while node_no != 0
      _, stuff = read_btree_node(node_no)
      for k, v in stuff[record_no..-1]
        if filter_key_function(k)
          kv << [k, v]
        else
          return kv
        end
      end

      node_no   = @last_btnode.fLink
      record_no = 0
    end
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
