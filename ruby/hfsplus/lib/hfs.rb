#!/usr/bin/env ruby

require "structs"
require "btree"

class HFSFile
  attr_reader :block_size

  def initialize(volume, hfsfork, fileID, deleted=false)
    @volume       = volume
    @block_size   = volume.block_size
    @fileID       = fileID
    @total_blocks = hfsfork.totalBlocks
    @logical_size = hfsfork.logicalSize
    @extents      = []
    @deleted      = deleted

    bc = 0;
    hfsfork.extentRecords.each { |extent|
      @extents << extent
      bc += extent.blockCount
    }

    while bc != hfsfork.totalBlocks
      key, value = @volume.get_extents_overflow_for_file(@fileID, bc)
      unless value
        puts "extents overflow missing, startblock=#{bc}"
        break
      end

      value.each { |ext| @extents << ext; bc += ext.blockCount }
    end
  end

  def read_all(out_file, truncate=true)
    f = File.open(out_file, "wb")
    0.upto(@total_blocks-1) { |i| f.write(read_block_at(i)) }
    f.truncate(@logicalSize) if truncate
    f.close
  end

  def read_all_buffer2(truncate=true)
    r = (0...@total_blocks).reduce("") { |m, i| m + self.read_block_at i }
    truncate ? r.slice(0, @logical_size) : r
  end

  def read_all_buffer(truncate=true)
    r = ""
    0.upto(@total_blocks-1) { |i| r += self.read_block_at i  }

    truncate ? r.slice(0, @logical_size) 
             : r
  end

  def read_block_at nth 
    bs = @volume.block_size
    raise Exception("BLOCK OUT OF BOUND ") if nth * bs > @logical_size
    bc = 0
    @extents.each do |extent| 
      bc += extent.blockCount 
      if nth < bc
        lba = extent.startBlock + (nth - (bc - extent.blockCount))
        if not @deleted and @fileID != KHFSAllocationFileID and
           not @volume.block_in_use?(lba)
           puts "FAIL, block ${n} not marked as used"
        end

        return @volume.read(lba * bs, bs)
      end
    end
    ""
  end
end

class HFSVolume
  attr_reader :header, :block_size

  def initialize(filename, write_=false, offset=0)
    @file = File.open(filename, "rb")
    @offset = offset
    @write_flag = write_

    begin
      data = read(0, 0x1000)
      @header = HFSPlusVolumeHeader.read(data[0x400...0x800])
    rescue
      raise Exception "exception while reading header"
      @file.close
    ensure
    end

    @block_size = @header.blockSize
    unless File.size(filename) == @block_size * @header.totalBlocks
      puts "WARNING: image appears to be truncated" 
    end

    @allocation_file   = HFSFile.new(self, @header.allocationFile, KHFSAllocationFileID)
    @allocation_bitmap = @allocation_file.read_all_buffer
    @extents_file = HFSFile.new(self, @header.extentsFile, KHFSExtentsFileID)
    @extents_tree = ExtentsOverflowTree.new(@extents_file)
#     @catalog_file = HFSFile.new(self, @header.catalogFile, KHFSCatalogFileID)
#     @xattr_file   = HFSFile.new(self, @header.attributesFile, KFHSAttributesFileID)
#     @catalog_tree = CatalogTree.new(@catalog_file)
#     @xattr_tree   = AttributesTree.new(@xattr_file)
#     @has_journal  = @header.attributes & ( 1 << KHFSVolumeJournaledBit)
  end

  def block_in_use? block
    this_byte = @allocation_bitmap[block / 8].ord
    return (this_byte & (1 << (7 - (block % 8)))) != 0
  end

  # Volume level read is different from that of HFSFile
  def read(offset, size)
    @file.seek(@offset + offset, IO::SEEK_SET)
    @file.read(size)
  end

  def write(offset, data)
    if @write_flag
      @file.seek(@offset + offset, IO::SEEK_SET)
      @file.write(data)
    end
  end

  def write_block(lba, block)
    @file.write(lba * @block_size, block)
  end

  def volume_id
    [@header.finderInfo[6], @header.finderInfo[7]].pack("NN")
  end

  def unallocated_blocks
    0.upto(@header.totalBlocks-1) { |nth|
      yield nth, read(nth * @block_size,  @block_size) unless block_in_use? nth
    }
  end

  def get_extents_overflow_for_file(fileID, forkType, startBlock)
    @extents_tree.search_extents(fileID, forkType, startBlock)
  end

  def get_xattr(fileID, name)
    @xattr_tree.search_extents(fileID, name)
  end

  def get_file_by_path(path)
    @catalog_tree.get_record_from_path(path)
  end

  def list_folder_contents(path)
    key, value = @catalog_tree.get_record_from_path(path)
    return if key.nil? or value.recordType != KHFSPlusFolderRecord

    @catalog_tree.get_folder_contents(value.folderID).each { |k, v|
      puts "folder" if v.recordType == KHFSPlusFolderRecord
      puts "file"   if v.recordType == KHFSPlusFileRecord
    }
  end

  def list_xattrs(path)
    k, v = @catalog_tree.get_record_from_path(path)
    return @xattr_tree.get_all_xattrs(v.fileID)   if k and v.recordType == KHFSPlusFileRecord
    return @xattr_tree.get_all_xattrs(v.folderID) if k and v.recordType == KHFSPlusFolderRecord
  end

  def read_file(path, return_str=false)
    k, v = @catalog_tree.get_record_from_path(path)
    if v.nil?
      puts "File ${path} not found"
      return
    end

    return unless v.recordType == KHFSPlusFileRecord
    xattr = get_xattr(v.fileID, "com.apple.decmpfs")
    if xattr
      puts "compressed"
    end

    f = HFSFile.new(self, v.dataFork, v.fileID)
    return return_str ? f.read_all_buffer : f.read_all(path)
  end

  def read_journal
    jb  = read(@header.journalInfoBlock * @block_size, @block_size)
    jib = JournalInfoBlock.read(jb)
    return read(jib.offset, jib.size)
  end
end

if __FILE__ == $PROGRAM_NAME
#   v = HFSVolme.new("data/HFSPlus.dmg", offset=0x40)
#   v.listFolderContents("/")
#   puts v.read_file("/usr/local/share/restore/imeisv_svn.plist")
#   puts v.list_xattrs("/usr/local/share/restore/imeisv_svn.plist")
end
