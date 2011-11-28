#!/usr/bin/env ruby

require "structs"

class HFSFile
  def initialize(volume, hfsfork, fileID, deleted=false)
    @volume       = volume
    @block_size   = volume.block_size
    @fileID       = fileID
    @total_blocks = hfsfork.totalBlocks
    @logical_size = hfsfork.logicalSize
    @extents      = []
    @deleted      = deleted

    bc = 0
    hfsfork.extentRecords.each { |extent|
      @extents << extent
      bc += extent.blockCount
    }

    until bc == hfsfork.totalBlocks
      key, value = @volume.get_extents_overflow_for_file(@fileID, bc)
      unless value
        puts "extents overflow missing, startblock=#{bc}"
        break
      end

      value.each { |extent| 
        @extents << extent; 
        bc += extent.blockCount
      }
    end
  end

  def read_all(output_file, truncate=true)
  end

  def read_all_buffer(truncate=true)
    r = ""
    0.upto(@total_blocks-1) { |i| r += self.read_block_at(i) }
    r = r.first(@logical_size) if truncate

    return r
  end

  def read_block_at(nth)
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
        @volume.read(lba * bs, bs)
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
      @header = HFSPlusVolumeHeader.read(data[0x400..0x800])
    rescue
      raise Exception "exception while reading header"
    ensure
      @file.close
    end

    @block_size = @header.blockSize
    unless File.size(filename) == @block_size * @header.totalBlocks
      puts "WARNING: image appears to be truncated" 
    end

    @allocation_file   = HFSFile.new(self, @header.allocationFile, KHFSAllocationFileID)
    @allocation_bitmap = @allocation_file.read_all_buffer
    @extents_file = HFSFile.new(self, @header.extentsFile, KHFSExtentsFileID)
    @extents_tree = ExtentsOverflowTree.new(@extents_file)
    @catalog_file = HFSFile.new(self, @header.catalogFile, KHFSCatalogFileID)
    @xattr_file   = HFSFile.new(self, @header.attributesFile, KFHSAttributesFileID)
    @catalog_tree = CatalogTree.new(@catalog_file)
    @xattr_tree   = AttributesTree.new(@xattr_file)
    @has_journal  = @header.attributes & ( 1 << KHFSVolumeJournaledBit)
  end

  def block_in_use? block
    this_byte = @allocation_bitmap[block / 8]
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
  end

  def get_extents_overflow_for_file
  end

  def get_xattr
  end

  def get_file_by_path(path)
  end

  def list_folder_contents(path)
  end

  def list_xattrs(path)
  end

  def read_file(path, return_str=false)
  end

  def read_journal
    jb  = read(@header.journalInfoBlock * @block_size, @block_size)
    jib = JournalInfoBlock.read(jb)
    return read(jib.offset, jib.size)
  end
end

if __FILE__ == $PROGRAM_NAME
  v = HFSVolme.new("myramdisk.dmg", offset=0x40)
  v.listFolderContents("/")
  puts v.read_file("/usr/local/share/restore/imeisv_svn.plist")
  puts v.list_xattrs("/usr/local/share/restore/imeisv_svn.plist")
end
